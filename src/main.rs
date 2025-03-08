#![no_std]
#![no_main]

mod basic;

use core::char;

use arduino_hal::prelude::*;
use arrayvec::{ArrayString, ArrayVec};
use basic::{
    BasicCommand, BasicControlFlow, BasicLine, Expression, InterpretationError, ParseError, Token,
};
use panic_halt as _;
use ufmt::{uwrite, uwriteln};

const RETURN_ASCII: u8 = b'\r';
const BACKSPACE_ASCII: u8 = b'\x7f';
const PROGRAM_LENGTH: usize = 10;
pub type Serial = arduino_hal::hal::usart::Usart0<arduino_hal::DefaultClock>;

fn list(serial: &mut Serial, program: &[Option<BasicCommand>]) {
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
                    uwrite!(serial, "PRINT \"").unwrap_infallible();
                    match expr {
                        Expression::String(str) => str
                            .chars()
                            .for_each(|c| uwrite!(serial, "{}", c).unwrap_infallible()),
                        Expression::Math(_, _) => {
                            uwrite!(serial, "<MATH EXPRESSION>").unwrap_infallible()
                        }
                    }
                    uwrite!(serial, "\"").unwrap_infallible();
                }
                _ => uwrite!(serial, "UNIMPLEMENTED PRINT").unwrap_infallible(),
            }

            uwriteln!(serial, "").unwrap_infallible();
        }
    });
}
#[arduino_hal::entry]
fn main() -> ! {
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let mut serial: Serial = arduino_hal::default_serial!(dp, pins, 57200);

    uwriteln!(&mut serial, "Starting TinyBASIC...\r").unwrap_infallible();
    uwriteln!(
        &mut serial,
        "program buffer size: {}\r",
        size_of::<[Option<BasicCommand>; PROGRAM_LENGTH]>()
    )
    .unwrap_infallible();
    uwriteln!(&mut serial, "variables size: {}\r", size_of::<[u8; 26]>()).unwrap_infallible();
    uwriteln!(
        &mut serial,
        "input buffer size: {}\r",
        size_of::<ArrayString<64>>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "token buffer size: {}\r",
        size_of::<ArrayVec<Token, 8>>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "number buffer size: {}\r",
        size_of::<Option<u8>>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "string buffer size: {}\r",
        size_of::<Option<ArrayString<32>>>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "keyword buffer size: {}\r",
        size_of::<Option<ArrayString<6>>>()
    )
    .unwrap_infallible();

    let mut program: [Option<BasicCommand>; PROGRAM_LENGTH] = [const { None }; PROGRAM_LENGTH];
    let mut program_counter: Option<usize> = None;
    let mut variables = [0u8; 26];

    let mut input_buffer = ArrayString::<64>::new();
    uwriteln!(&mut serial, "READY\r",).unwrap_infallible();

    loop {
        if let Some(counter) = program_counter {
            let mut next_count = counter + 1;

            if let Some(command) = &program[counter] {
                match command.execute(&mut serial, &mut variables) {
                    BasicControlFlow::Run => (),
                    BasicControlFlow::Goto(line) => next_count = line as usize,
                    BasicControlFlow::End => next_count = PROGRAM_LENGTH,
                    BasicControlFlow::List => list(&mut serial, &program),
                    BasicControlFlow::Clear => program = [const { None }; PROGRAM_LENGTH],
                    BasicControlFlow::Continue => (),
                }
            }

            if next_count >= PROGRAM_LENGTH {
                program_counter = None;
                variables = [0u8; 26];
            } else {
                _ = program_counter.insert(next_count);
            }
        } else {
            uwrite!(&mut serial, "> ").unwrap_infallible();
            loop {
                let char_u8 = serial.read_byte();
                //uwrite!(&mut serial, "{}", char_u8).unwrap_infallible();
                match char_u8 {
                    RETURN_ASCII => {
                        uwriteln!(&mut serial, "\r").unwrap_infallible();
                        match Token::tokenize(&input_buffer) {
                            Ok(tokens) => {
                                let basic_line = BasicLine::from_tokens(tokens);
                                match basic_line {
                                    Ok(line) => {
                                        if line.is_immeadiate() {
                                            match line.execute(&mut serial, &mut variables) {
                                                BasicControlFlow::Run => {
                                                    uwriteln!(&mut serial, "starting program...")
                                                        .unwrap_infallible();
                                                    program_counter = Some(0);
                                                }
                                                BasicControlFlow::List => {
                                                    list(&mut serial, &program);
                                                }
                                                BasicControlFlow::Clear => {
                                                    program = [const { None }; PROGRAM_LENGTH];
                                                }
                                                _ => (),
                                            }
                                        } else if line.line_num.unwrap_or(0) < PROGRAM_LENGTH {
                                            program[line.line_num.unwrap_or(0)] =
                                                Some(line.command);
                                        } else {
                                            uwriteln!(&mut serial, "line num too much")
                                                .unwrap_infallible();
                                        }
                                    }
                                    Err(e) => match e {
                                        InterpretationError::NoArgs => {
                                            uwriteln!(&mut serial, "arguments missing")
                                        }
                                        InterpretationError::UnexpectedArgs => {
                                            uwriteln!(&mut serial, "arguments unexpected")
                                        }
                                        InterpretationError::UnimplementedToken => {
                                            uwriteln!(&mut serial, "unimplemented token")
                                        }
                                        InterpretationError::StackLimitReached => {
                                            uwriteln!(&mut serial, "math stack limit reached")
                                        }
                                    }
                                    .unwrap_infallible(),
                                }
                            }
                            Err(e) => match e {
                                ParseError::TooManyTokens => {
                                    uwriteln!(&mut serial, "too many tokens\r").unwrap_infallible()
                                }
                                ParseError::Malformed => {
                                    uwriteln!(&mut serial, "malformed\r").unwrap_infallible()
                                }
                                ParseError::Capacity => {
                                    uwriteln!(&mut serial, "str cap full\r").unwrap_infallible();
                                }
                                ParseError::NumberOverflow => {
                                    uwriteln!(&mut serial, "number overflow: max is {}", u8::MAX)
                                        .unwrap_infallible();
                                }
                            },
                        }
                        input_buffer.clear();
                        break;
                    }
                    BACKSPACE_ASCII => {
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
