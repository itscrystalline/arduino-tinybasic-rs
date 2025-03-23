#![no_std]
#![no_main]

mod arduino;
mod basic;

use core::char;

use arduino::{EepromError, Serial};
use arduino_hal::prelude::*;
use arrayvec::ArrayString;
use avr_progmem::progmem;
use avr_progmem::progmem_display as D;
use basic::{
    expression::Expression,
    interpreter::{BasicCommand, BasicControlFlow, BasicLine, InterpretationError, MathToken},
    lexer::{ParseError, String, Token, TokenBuffer},
};
use panic_halt as _;
use ufmt::{uwrite, uwriteln};

const RETURN_ASCII: u8 = b'\r';
const BACKSPACE_ASCII_SERIAL: u8 = b'\x08';
const BACKSPACE_ASCII_QEMU: u8 = b'\x7f';

#[cfg(not(feature = "full"))]
const PROGRAM_LENGTH: usize = 32;
#[cfg(feature = "full")]
const PROGRAM_LENGTH: usize = 10;

#[cfg(feature = "full")]
fn print_sizes(serial: &mut Serial) {
    uwriteln!(
        serial,
        "prog buf size: {}\r",
        size_of::<[Option<BasicCommand>; PROGRAM_LENGTH]>()
    )
    .unwrap_infallible();
    uwriteln!(serial, "vars size: {}\r", size_of::<[usize; 26]>()).unwrap_infallible();
    uwriteln!(serial, "input buf size: {}\r", size_of::<ArrayString<32>>()).unwrap_infallible();
    uwriteln!(serial, "token buf size: {}\r", size_of::<TokenBuffer>()).unwrap_infallible();
    uwriteln!(serial, "num buf size: {}\r", size_of::<Option<usize>>()).unwrap_infallible();
    uwriteln!(serial, "str buf size: {}\r", size_of::<Option<String>>()).unwrap_infallible();
    uwriteln!(
        serial,
        "str table size: {}\r",
        size_of::<[Option<String>; 10]>()
    )
    .unwrap_infallible();
    uwriteln!(
        serial,
        "kw buffer size: {}\r",
        size_of::<Option<ArrayString<6>>>()
    )
    .unwrap_infallible();
    uwriteln!(serial, "arch word size: {} bytes\r", size_of::<usize>()).unwrap_infallible();
}

progmem! {
    static progmem string GREET = "Starting TinyBASIC...\r";
    static progmem string GREET2 = "(made by @itscrystalline on github, source code at https://github.com/itscrystalline/arduino-tinybasic-rs)\r";
    static progmem string READY = "READY\r";
    static progmem string PROMPT = "> ";

    pub static progmem string E_LINE_TOO_MUCH = "lin2much\r";
    pub static progmem string E_ARGS_MISSING = "no args\r";
    pub static progmem string E_ARGS_UNEXPECTED = "bad args\r";
    pub static progmem string E_UNIMPLEMENTED_TOKEN = "no token\r";
    pub static progmem string E_MATH_STACK = "mathfull\r";
    pub static progmem string E_STR_TAB_FULL = "strfull\r";
    pub static progmem string E_MATH_IN_BOOL = "mathbool\r";
    pub static progmem string E_TOKENS = "2manytok\r";
    pub static progmem string E_MALFORMED = "badform\r";
    pub static progmem string E_STR_CAP_FULL = "strcap\r";
    pub static progmem string E_NUM_OVERFLOW = "numovfl\r";
    pub static progmem string E_UNIMPLEMENTED_PRINT = "noprint\r";
    pub static progmem string E_INCOMPLETE_EXPR = "incomplete\r";
    pub static progmem string E_VAL_OVERFLOW = "valovfl\r";
    pub static progmem string E_VAL_UNDERFLOW = "valundr\r";
    pub static progmem string E_DIV_ZERO = "div0\r";
    pub static progmem string E_NUM_STACK_FULL = "numstack\r";
    pub static progmem string E_UNIMPLEMENTED = "noimpl\r";
    pub static progmem string E_PIN_NOT_INPUT = "notin\r";
    pub static progmem string E_PIN_NOT_OUTPUT = "notout\r";
    pub static progmem string E_PIN_NOT_PWM = "notpwm\r";
    pub static progmem string E_PIN_UNUSABLE = "badpin\r";
    pub static progmem string E_NOT_BOOL = "not01\r";
    pub static progmem string E_INVALID_DUTY = "badduty\r";
    pub static progmem string E_SAVE_PROGRAM = "nosave\r";
    pub static progmem string E_SAVE_STRING_TABLE = "nostrsv\r";
    pub static progmem string E_LOAD_PROGRAM = "noload\r";
    pub static progmem string E_LOAD_STRING_TABLE = "nostrld\r";
}

pub type BasicProgram = [Option<BasicCommand>; PROGRAM_LENGTH];

#[arduino_hal::entry]
fn main() -> ! {
    let (mut pins, mut serial, mut eeprom) = arduino::init();

    uwriteln!(&mut serial, "{}", GREET).unwrap_infallible();
    uwriteln!(&mut serial, "{}", GREET2).unwrap_infallible();

    #[cfg(feature = "full")]
    print_sizes(&mut serial);

    let mut program: BasicProgram = [const { None }; PROGRAM_LENGTH];
    let mut program_counter: Option<usize> = None;
    let mut variables = [0usize; 26];
    let mut string_table: [Option<String>; 8] = [None; 8];

    let mut tokens = TokenBuffer::new();
    let mut number_buffer: Option<usize> = None;
    let mut string_buffer: Option<String> = None;
    let mut keyword_buffer: Option<ArrayString<6>> = None;

    let mut input_buffer = ArrayString::<32>::new();
    uwriteln!(&mut serial, "{}", READY).unwrap_infallible();

    loop {
        if let Some(counter) = program_counter {
            let mut next_count = counter + 1;

            if let Some(command) = &program[counter] {
                match command.execute(&mut serial, &mut pins, &mut variables, &string_table) {
                    BasicControlFlow::Goto(line) => next_count = line,
                    BasicControlFlow::End => next_count = PROGRAM_LENGTH,
                    BasicControlFlow::List => {
                        list(&mut serial, &program, &variables, &string_table)
                    }
                    BasicControlFlow::Clear => {
                        clear(&mut program, &mut variables, &mut string_table);
                    }
                    BasicControlFlow::Save
                    | BasicControlFlow::Load
                    | BasicControlFlow::Run
                    | BasicControlFlow::Continue => (),
                }
            }

            if next_count >= PROGRAM_LENGTH {
                program_counter = None;
                uwriteln!(&mut serial, "\r\n{}", READY).unwrap_infallible();
            } else {
                _ = program_counter.insert(next_count);
            }
        } else {
            uwrite!(&mut serial, "{}", PROMPT).unwrap_infallible();
            loop {
                let char_u8 = serial.read_byte();
                //uwrite!(&mut serial, "{} ", char_u8).unwrap_infallible();
                match char_u8 {
                    RETURN_ASCII => {
                        uwriteln!(&mut serial, "\r").unwrap_infallible();
                        match Token::tokenize(
                            &input_buffer,
                            &mut tokens,
                            &mut number_buffer,
                            &mut string_buffer,
                            &mut keyword_buffer,
                        ) {
                            Ok(()) => {
                                let basic_line = BasicLine::from_tokens(
                                    &mut tokens,
                                    &mut program,
                                    &mut string_table,
                                );
                                match basic_line {
                                    Ok(line) => {
                                        if line.is_immeadiate() {
                                            match line.execute(
                                                &mut serial,
                                                &mut pins,
                                                &mut variables,
                                                &string_table,
                                            ) {
                                                BasicControlFlow::Run => {
                                                    program_counter = Some(0);
                                                }
                                                BasicControlFlow::List => {
                                                    list(
                                                        &mut serial,
                                                        &program,
                                                        &variables,
                                                        &string_table,
                                                    );
                                                }
                                                BasicControlFlow::Clear => {
                                                    clear(
                                                        &mut program,
                                                        &mut variables,
                                                        &mut string_table,
                                                    );
                                                    uwriteln!(&mut serial, "\r\n{}", READY)
                                                        .unwrap_infallible();
                                                }
                                                BasicControlFlow::Save => {
                                                    if let Err(e) = arduino::eeprom_save(
                                                        &mut serial,
                                                        &mut eeprom,
                                                        &program,
                                                        &string_table,
                                                    ) {
                                                        match e {
                                                            EepromError::SaveProgram => uwriteln!(
                                                                &mut serial,
                                                                "{}",
                                                                E_SAVE_PROGRAM
                                                            ),
                                                            EepromError::SaveString => uwriteln!(
                                                                &mut serial,
                                                                "{}",
                                                                E_SAVE_STRING_TABLE
                                                            ),
                                                            _ => unreachable!(),
                                                        }
                                                        .unwrap_infallible()
                                                    }
                                                }
                                                BasicControlFlow::Load => {
                                                    if let Err(e) = arduino::eeprom_load(
                                                        &mut serial,
                                                        &eeprom,
                                                        &mut program,
                                                        &mut string_table,
                                                    ) {
                                                        match e {
                                                            EepromError::LoadProgram => {
                                                                uwriteln!(
                                                                    &mut serial,
                                                                    "{}",
                                                                    E_LOAD_PROGRAM
                                                                )
                                                            }
                                                            EepromError::LoadString => uwriteln!(
                                                                &mut serial,
                                                                "{}",
                                                                E_LOAD_STRING_TABLE
                                                            ),
                                                            _ => unreachable!(),
                                                        }
                                                        .unwrap_infallible()
                                                    }
                                                }
                                                _ => (),
                                            }
                                        } else if line.line_num.unwrap_or(0) < PROGRAM_LENGTH {
                                            program[line.line_num.unwrap_or(0)] =
                                                Some(line.command);
                                        } else {
                                            uwriteln!(&mut serial, "{}", E_LINE_TOO_MUCH)
                                                .unwrap_infallible();
                                        }
                                    }
                                    Err(e) => match e {
                                        InterpretationError::NoArgs => {
                                            uwriteln!(&mut serial, "{}", E_ARGS_MISSING)
                                        }
                                        InterpretationError::UnexpectedArgs => {
                                            uwriteln!(&mut serial, "{}", E_ARGS_UNEXPECTED)
                                        }
                                        InterpretationError::UnimplementedToken => {
                                            uwriteln!(&mut serial, "{}", E_UNIMPLEMENTED_TOKEN)
                                        }
                                        InterpretationError::StackFull => {
                                            uwriteln!(&mut serial, "{}", E_MATH_STACK)
                                        }
                                        InterpretationError::StringTableFull => {
                                            uwriteln!(&mut serial, "{}", E_STR_TAB_FULL)
                                        }
                                        InterpretationError::MathToBooleanFailed => {
                                            uwriteln!(&mut serial, "{}", E_MATH_IN_BOOL)
                                        }
                                    }
                                    .unwrap_infallible(),
                                }
                            }
                            Err(e) => match e {
                                ParseError::TooManyTokens => {
                                    uwriteln!(&mut serial, "{}", E_TOKENS).unwrap_infallible()
                                }
                                ParseError::Malformed => {
                                    uwriteln!(&mut serial, "{}", E_MALFORMED).unwrap_infallible()
                                }
                                ParseError::Capacity => {
                                    uwriteln!(&mut serial, "{}", E_STR_CAP_FULL)
                                        .unwrap_infallible();
                                }
                                ParseError::NumberOverflow => {
                                    uwriteln!(&mut serial, "{}[{}]\r", E_NUM_OVERFLOW, usize::MAX)
                                        .unwrap_infallible();
                                }
                            },
                        }
                        input_buffer.clear();
                        tokens.clear();
                        number_buffer = None;
                        string_buffer = None;
                        keyword_buffer = None;
                        break;
                    }
                    BACKSPACE_ASCII_SERIAL | BACKSPACE_ASCII_QEMU => {
                        if input_buffer.pop().is_some() {
                            uwrite!(&mut serial, "\r\n{}", PROMPT).unwrap_infallible();
                            input_buffer
                                .chars()
                                .for_each(|c| uwrite!(&mut serial, "{}", c).unwrap_infallible());
                        }
                    }
                    _ => {
                        let c = (char_u8 as char).to_ascii_uppercase();
                        if input_buffer.try_push(c).is_ok() {
                            uwrite!(&mut serial, "{}", c).unwrap_infallible()
                        }
                    }
                }
            }
        }
    }
}

pub fn put_string_table(
    string: ArrayString<16>,
    string_table: &mut [Option<String>],
) -> Result<u8, ArrayString<16>> {
    for (idx, op) in string_table.iter_mut().enumerate() {
        if op.is_none() {
            *op = Some(string);
            return Ok(idx as u8);
        }
    }
    Err(string)
}

pub fn clear(
    program: &mut BasicProgram,
    variables: &mut [usize],
    string_table: &mut [Option<String>],
) {
    program.iter_mut().for_each(|c| *c = None);
    string_table.iter_mut().for_each(|c| *c = None);
    variables.iter_mut().for_each(|c| *c = 0);
}

fn list(
    serial: &mut Serial,
    program: &BasicProgram,
    _variables: &[usize],
    string_table: &[Option<String>],
) {
    program.iter().enumerate().for_each(|(line_num, l)| {
        if let Some(line) = l {
            uwrite!(serial, "{} ", line_num).unwrap_infallible();

            match line {
                BasicCommand::List => uwriteln!(serial, "{}", D!("LIST\r")).unwrap_infallible(),
                BasicCommand::End => uwriteln!(serial, "{}", D!("END\r")).unwrap_infallible(),
                BasicCommand::Run => uwriteln!(serial, "{}", D!("RUN\r")).unwrap_infallible(),
                BasicCommand::Rem => uwriteln!(serial, "{}", D!("REM\r")).unwrap_infallible(),
                BasicCommand::Goto(line_num) if line_num.is_some() => {
                    uwriteln!(serial, "{}{}\r", D!("GOTO "), line_num.unwrap()).unwrap_infallible()
                }
                BasicCommand::Print(Some(expr)) => {
                    uwrite!(serial, "{}", D!("PRINT ")).unwrap_infallible();
                    match expr {
                        Expression::String(str) => {
                            uwrite!(serial, "\"").unwrap_infallible();
                            string_table[*str as usize]
                                .unwrap()
                                .chars()
                                .for_each(|c| uwrite!(serial, "{}", c).unwrap_infallible());
                            uwrite!(serial, "\"\r\n").unwrap_infallible();
                        }
                        Expression::Math(_) => {
                            uwriteln!(serial, "{}\r", D!("<MATHEXPR>")).unwrap_infallible()
                        }
                        Expression::Boolean(_, Some(rel), _) => uwriteln!(
                            serial,
                            "{}{}{}\r",
                            D!("<MATHEXPR> "),
                            rel,
                            D!(" <MATHEXPR>")
                        )
                        .unwrap_infallible(),
                        _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
                    }
                }
                BasicCommand::Let(Some(var_idx), Some(_)) => {
                    uwriteln!(
                        serial,
                        "{}{}{}",
                        D!("LET "),
                        (*var_idx + b'A') as char,
                        D!(" = <MATHEXPR>\r")
                    )
                    .unwrap_infallible();
                }
                BasicCommand::If(
                    Expression::Boolean(Some(lhs), Some(op), Some(rhs)),
                    Some(goto_dest),
                ) => {
                    uwriteln!(serial, "IF {} {} {} THEN {}\r", lhs, op, rhs, goto_dest)
                        .unwrap_infallible();
                }
                BasicCommand::MakeInput(Some(pin)) => {
                    uwriteln!(serial, "{}{}\r", D!("MKIN "), pin).unwrap_infallible()
                }
                BasicCommand::MakeOutput(Some(pin)) => {
                    uwriteln!(serial, "{}{}\r", D!("MKOUT "), pin).unwrap_infallible()
                }
                BasicCommand::AnalogRead(Some(pin), Some(MathToken::Variable(idx)))
                | BasicCommand::DigitalRead(Some(pin), Some(MathToken::Variable(idx))) => {
                    match line {
                        BasicCommand::AnalogRead(_, _) => uwrite!(serial, "A").unwrap_infallible(),
                        BasicCommand::DigitalRead(_, _) => uwrite!(serial, "D").unwrap_infallible(),
                        _ => (),
                    }
                    uwriteln!(serial, "{}{} {}\r", D!("READ "), pin, (idx + b'A') as char)
                        .unwrap_infallible()
                }
                BasicCommand::AnalogWrite(Some(pin), Some(token))
                | BasicCommand::DigitalWrite(Some(pin), Some(token)) => {
                    match line {
                        BasicCommand::AnalogWrite(_, _) => uwrite!(serial, "A").unwrap_infallible(),
                        BasicCommand::DigitalWrite(_, _) => {
                            uwrite!(serial, "D").unwrap_infallible()
                        }
                        _ => (),
                    }
                    uwrite!(serial, "{}{} ", D!("WRITE "), pin).unwrap_infallible();
                    match token {
                        MathToken::Variable(idx) => {
                            uwriteln!(serial, "{}\r", (idx + b'A') as char).unwrap_infallible()
                        }
                        MathToken::Literal(num) => {
                            uwriteln!(serial, "{}\r", num).unwrap_infallible()
                        }
                        _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
                    }
                }
                BasicCommand::DelayMs(Some(ms)) => {
                    uwrite!(serial, "{}", D!("DELAY ")).unwrap_infallible();
                    match ms {
                        MathToken::Variable(idx) => {
                            uwriteln!(serial, "{}\r", (idx + b'A') as char).unwrap_infallible()
                        }
                        MathToken::Literal(num) => {
                            uwriteln!(serial, "{}\r", num).unwrap_infallible()
                        }
                        _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
                    }
                }
                _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
            }
        }
    });
    uwriteln!(serial, "\r\n{}", READY).unwrap_infallible();
}
