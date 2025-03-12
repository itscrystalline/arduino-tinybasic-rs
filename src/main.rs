#![no_std]
#![no_main]

mod arduino;
mod basic;

use core::char;

use arduino::Serial;
use arduino_hal::prelude::*;
use arrayvec::ArrayString;
use avr_progmem::progmem;
use basic::{
    expression::Expression,
    interpreter::{BasicCommand, BasicControlFlow, BasicLine, InterpretationError},
    lexer::{ParseError, String, Token, TokenBuffer},
};
use panic_halt as _;
use ufmt::{uwrite, uwriteln};

const RETURN_ASCII: u8 = b'\r';
const BACKSPACE_ASCII_SERIAL: u8 = b'\x08';
const BACKSPACE_ASCII_QEMU: u8 = b'\x7f';

#[cfg(not(feature = "full"))]
const PROGRAM_LENGTH: usize = 40;
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
    static progmem string READY = "READY\r";
    static progmem string PROMPT = "> ";

    pub static progmem string E_LINE_TOO_MUCH = "line num too much\r";
    pub static progmem string E_ARGS_MISSING = "arguments missing\r";
    pub static progmem string E_ARGS_UNEXPECTED = "arguments unexpected\r";
    pub static progmem string E_UNIMPLEMENTED_TOKEN = "unimplemented token\r";
    pub static progmem string E_MATH_STACK = "math stack limit reached\r";
    pub static progmem string E_STR_TAB_FULL = "string table full\r";
    pub static progmem string E_MATH_IN_BOOL = "too much math in bool expr\r";
    pub static progmem string E_TOKENS = "too many tokens\r";
    pub static progmem string E_MALFORMED = "malformed\r";
    pub static progmem string E_STR_CAP_FULL = "str cap full\r";
    pub static progmem string E_NUM_OVERFLOW = "number overflow: max is ";
    pub static progmem string E_UNIMPLEMENTED_PRINT = "UNIMPLEMENTED PRINT\r";
    pub static progmem string E_INCOMPLETE_EXPR = "incomplete expression\r";
    pub static progmem string E_VAL_OVERFLOW = "value overflowed\r";
    pub static progmem string E_VAL_UNDERFLOW = "value underflowed\r";
    pub static progmem string E_DIV_ZERO = "division by zero\r";
    pub static progmem string E_NUM_STACK_FULL = "number stack full\r";
    pub static progmem string E_UNIMPLEMENTED = "UNIMPLEMENTED\r";
    pub static progmem string E_PIN_NOT_INPUT = "pin is not input\r";
    pub static progmem string E_PIN_NOT_OUTPUT = "pin is not output\r";
    pub static progmem string E_PIN_NOT_PWM = "pin is not pwm\r";
    pub static progmem string E_PIN_UNUSABLE = "pin is not usable\r";
    pub static progmem string E_PIN_RESERVED = "pin is reserved\r";
    pub static progmem string E_NOT_BOOL = "not 0 or 1\r";
}

#[arduino_hal::entry]
fn main() -> ! {
    let (mut pins, mut serial) = arduino::init();

    uwriteln!(&mut serial, "{}", GREET).unwrap_infallible();

    #[cfg(feature = "full")]
    print_sizes(&mut serial);

    let mut program: [Option<BasicCommand>; PROGRAM_LENGTH] = [const { None }; PROGRAM_LENGTH];
    let mut program_counter: Option<usize> = None;
    let mut variables = [0usize; 26];
    let mut string_table: [Option<ArrayString<16>>; 10] = [None; 10];

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
                    BasicControlFlow::Run => (),
                    BasicControlFlow::Goto(line) => next_count = line,
                    BasicControlFlow::End => next_count = PROGRAM_LENGTH,
                    BasicControlFlow::List => {
                        list(&mut serial, &program, &variables, &string_table)
                    }
                    BasicControlFlow::Clear => {
                        clear(&mut program, &mut variables, &mut string_table);
                    }
                    BasicControlFlow::Continue => (),
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
    program: &mut [Option<BasicCommand>],
    variables: &mut [usize],
    string_table: &mut [Option<String>],
) {
    program.iter_mut().for_each(|c| *c = None);
    string_table.iter_mut().for_each(|c| *c = None);
    variables.iter_mut().for_each(|c| *c = 0);
}

fn list(
    serial: &mut Serial,
    program: &[Option<BasicCommand>],
    variables: &[usize],
    string_table: &[Option<String>],
) {
    program.iter().enumerate().for_each(|(line_num, l)| {
        if let Some(line) = l {
            uwrite!(serial, "{} ", line_num).unwrap_infallible();

            match line {
                BasicCommand::List => uwriteln!(serial, "LIST\r").unwrap_infallible(),
                BasicCommand::End => uwriteln!(serial, "END\r").unwrap_infallible(),
                BasicCommand::Run => uwriteln!(serial, "RUN\r").unwrap_infallible(),
                BasicCommand::Rem => uwriteln!(serial, "REM\r").unwrap_infallible(),
                BasicCommand::Goto(line_num) if line_num.is_some() => {
                    uwriteln!(serial, "GOTO {}\r", line_num.unwrap()).unwrap_infallible()
                }
                BasicCommand::Print(Some(expr)) => {
                    uwrite!(serial, "PRINT ").unwrap_infallible();
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
                            uwriteln!(serial, "<MATHEXPR>\r").unwrap_infallible()
                        }
                        Expression::Boolean(_, Some(rel), _) => {
                            uwriteln!(serial, "<MATHEXPR> {} <MATHEXPR>\r", rel).unwrap_infallible()
                        }
                        _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
                    }
                }
                BasicCommand::Let(Some(var_idx), Some(_)) => {
                    uwriteln!(serial, "LET {} = <MATHEXPR>\r", (*var_idx + b'A') as char)
                        .unwrap_infallible();
                }
                BasicCommand::If(
                    Expression::Boolean(Some(lhs), Some(op), Some(rhs)),
                    Some(goto_dest),
                ) => {
                    uwriteln!(serial, "IF {} {} {} THEN {}\r", lhs, op, rhs, goto_dest)
                        .unwrap_infallible();
                }
                _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED_PRINT).unwrap_infallible(),
            }
        }
    });
    uwriteln!(serial, "\r\n{}", READY).unwrap_infallible();
}
