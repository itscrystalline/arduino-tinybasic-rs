#![no_std]
#![no_main]

mod arduino;
mod basic;

use core::char;

use arduino::Serial;
use arduino_hal::prelude::*;
use arrayvec::ArrayString;
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
const PROGRAM_LENGTH: usize = 32;
#[cfg(feature = "full")]
const PROGRAM_LENGTH: usize = 10;

#[cfg(feature = "full")]
const ERROR_TABLE: [&str; 24] = [
    "line num too much",
    "arguments missing",
    "arguments unexpected",
    "unimplemented token",
    "math stack limit reached",
    "string table full",
    "too much math in bool expr",
    "too many tokens",
    "malformed",
    "str cap full",
    "number overflow: max is ",
    "UNIMPLEMENTED PRINT",
    "incomplete expression",
    "value overflowed",
    "value underflowed",
    "division by zero",
    "number stack full",
    "UNIMPLEMENTED",
    "pin is not input",
    "pin is not output",
    "pin is not pwm",
    "pin is not usable",
    "pin is reserved",
    "not 0 or 1",
];

#[cfg(not(feature = "full"))]
const ERROR_TABLE: [&str; 24] = [
    "E00", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12",
    "E13", "E14", "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23",
];

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

#[arduino_hal::entry]
fn main() -> ! {
    let (mut pins, mut serial) = arduino::init();

    uwriteln!(&mut serial, "Starting TinyBASIC...\r").unwrap_infallible();

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
    uwriteln!(&mut serial, "READY\r",).unwrap_infallible();

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
                uwriteln!(&mut serial, "\r\nREADY\r").unwrap_infallible();
            } else {
                _ = program_counter.insert(next_count);
            }
        } else {
            uwrite!(&mut serial, "> ").unwrap_infallible();
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
                                                    uwriteln!(&mut serial, "\r\nOK\r")
                                                        .unwrap_infallible();
                                                }
                                                _ => (),
                                            }
                                        } else if line.line_num.unwrap_or(0) < PROGRAM_LENGTH {
                                            program[line.line_num.unwrap_or(0)] =
                                                Some(line.command);
                                        } else {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[0])
                                                .unwrap_infallible();
                                        }
                                    }
                                    Err(e) => match e {
                                        InterpretationError::NoArgs => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[1])
                                        }
                                        InterpretationError::UnexpectedArgs => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[2])
                                        }
                                        InterpretationError::UnimplementedToken => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[3])
                                        }
                                        InterpretationError::StackFull => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[4])
                                        }
                                        InterpretationError::StringTableFull => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[5])
                                        }
                                        InterpretationError::MathToBooleanFailed => {
                                            uwriteln!(&mut serial, "{}\r", ERROR_TABLE[6])
                                        }
                                    }
                                    .unwrap_infallible(),
                                }
                            }
                            Err(e) => match e {
                                ParseError::TooManyTokens => {
                                    uwriteln!(&mut serial, "{}\r", ERROR_TABLE[7])
                                        .unwrap_infallible()
                                }
                                ParseError::Malformed => {
                                    uwriteln!(&mut serial, "{}\r", ERROR_TABLE[8])
                                        .unwrap_infallible()
                                }
                                ParseError::Capacity => {
                                    uwriteln!(&mut serial, "{}\r", ERROR_TABLE[9])
                                        .unwrap_infallible();
                                }
                                ParseError::NumberOverflow => {
                                    uwriteln!(&mut serial, "{}[{}]\r", ERROR_TABLE[10], usize::MAX)
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
                            uwrite!(&mut serial, "\r\n> ").unwrap_infallible();
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
                BasicCommand::List => uwrite!(serial, "LIST").unwrap_infallible(),
                BasicCommand::End => uwrite!(serial, "END").unwrap_infallible(),
                BasicCommand::Run => uwrite!(serial, "RUN").unwrap_infallible(),
                BasicCommand::Rem => uwrite!(serial, "REM").unwrap_infallible(),
                BasicCommand::Goto(line_num) if line_num.is_some() => {
                    uwrite!(serial, "GOTO {}", line_num.unwrap()).unwrap_infallible()
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
                            uwrite!(serial, "\"").unwrap_infallible();
                        }
                        Expression::Math(_) => uwrite!(serial, "<MATHEXPR>").unwrap_infallible(),
                        Expression::Boolean(_, Some(rel), _) => {
                            uwrite!(serial, "<MATHEXPR> {} <MATHEXPR>", rel).unwrap_infallible()
                        }
                        _ => uwrite!(serial, "{}", ERROR_TABLE[11]).unwrap_infallible(),
                    }
                }
                BasicCommand::Let(Some(var_idx), Some(_)) => {
                    uwrite!(serial, "LET {} = <MATHEXPR>", (*var_idx + b'A') as char)
                        .unwrap_infallible();
                }
                BasicCommand::If(
                    Expression::Boolean(Some(lhs), Some(op), Some(rhs)),
                    Some(goto_dest),
                ) => {
                    uwrite!(serial, "IF {} {} {} THEN {}", lhs, op, rhs, goto_dest)
                        .unwrap_infallible();
                }
                _ => uwrite!(serial, "{}", ERROR_TABLE[11]).unwrap_infallible(),
            }
            uwriteln!(serial, "\r").unwrap_infallible();
        }
    });
    uwriteln!(serial, "\r\nOK\r").unwrap_infallible();
}
