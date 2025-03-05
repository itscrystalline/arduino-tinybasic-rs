#![no_std]
#![no_main]

use core::char;

use arduino_hal::prelude::_unwrap_infallible_UnwrapInfallible;
use arrayvec::{ArrayString, ArrayVec};
use panic_halt as _;
use ufmt::{uwrite, uwriteln};

const RETURN_ASCII: u8 = 13; //\r
const LINE_LENGTH: usize = 64;

struct BasicLine(u8, BasicCommand);
#[derive(Copy, Clone)]
enum BasicCommand {
    Print(ArrayString<32>),
    Rem,
}

enum Token {
    Number(u8),
    Keyword(Keyword),
    String(ArrayString<32>),
    RelationOperator(Operator),
    Variable(char),
}

enum Operator {
    Equal,
    Less,
    More,
    LessEqual,
    MoreEqual,
}

enum Keyword {
    Invalid,
    Print,
    If,
    Goto,
    Input,
    Let,
    GoSub,
    Return,
    Clear,
    List,
    Run,
    End,
}

impl From<ArrayString<6>> for Keyword {
    fn from(value: ArrayString<6>) -> Self {
        match value.as_str() {
            "PRINT" => Keyword::Print,
            "IF" => Keyword::If,
            "GOTO" => Keyword::Goto,
            "INPUT" => Keyword::Input,
            "LET" => Keyword::Let,
            "GOSUB" => Keyword::GoSub,
            "RETURN" => Keyword::Return,
            "CLEAR" => Keyword::Clear,
            "LIST" => Keyword::List,
            "RUN" => Keyword::Run,
            "END" => Keyword::End,
            _ => Keyword::Invalid,
        }
    }
}

enum ParseError {
    Malformed,
}

impl Token {
    fn tokenize(str: ArrayString<64>) -> Result<ArrayVec<Token, 64>, ParseError> {
        let mut tokens = ArrayVec::<Token, 64>::new();
        let mut number_buffer: Option<u8> = None;
        let mut string_buffer: Option<ArrayString<32>> = None;
        let mut keyword_buffer: Option<ArrayString<6>> = None;
        for (idx, ch) in str.char_indices() {
            match ch {
                ' ' => {
                    if let Some(number) = number_buffer {
                        tokens.push(Token::Number(number));
                        number_buffer = None;
                    }
                    if let Some(keyword) = keyword_buffer {
                        match Keyword::from(keyword) {
                            Keyword::Invalid => return Err(ParseError::Malformed),
                            kw => tokens.push(Token::Keyword(kw)),
                        }
                    }
                }
                '"' => match string_buffer {
                    Some(string) => {
                        tokens.push(Token::String(string));
                        string_buffer = None;
                    }
                    None => string_buffer = Some(ArrayString::zero_filled()),
                },
                num_ch if num_ch.is_ascii_digit() => {
                    let num = num_ch as u8 - 48;
                    match (number_buffer, string_buffer) {
                        (Some(ref mut number), None) => *number = *number * 10 + num,
                        (None, Some(ref mut str)) => str.push(num_ch),
                        (None, None) => number_buffer = Some(num),
                        _ => unreachable!(),
                    }
                }
                char => match string_buffer {
                    Some(mut string) => string.push(char),
                    None => match keyword_buffer {
                        Some(mut keyword) => keyword.push(char),
                        None => {
                            let mut buf = ArrayString::<6>::new();
                            buf.push(char);
                            keyword_buffer = Some(buf);
                        }
                    },
                },
            }
        }

        Ok(tokens)
    }
}

#[arduino_hal::entry]
fn main() -> ! {
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let mut serial = arduino_hal::default_serial!(dp, pins, 57200);

    uwriteln!(&mut serial, "Starting TinyBASIC...").unwrap_infallible();

    let program_buffer: [Option<BasicCommand>; 128] = [None; 128];

    loop {
        let mut input_buffer = ArrayString::<64>::zero_filled();
        loop {
            let char_u8 = serial.read_byte();
            match char_u8 {
                RETURN_ASCII => {
                    uwriteln!(&mut serial, "").unwrap_infallible();
                    uwrite!(&mut serial, "> ").unwrap_infallible();
                    break;
                }
                _ => {
                    let c = (char_u8 as char).to_ascii_uppercase();
                    if input_buffer.try_push(c).is_ok() {
                        uwrite!(&mut serial, "{}", c).unwrap_infallible();
                    }
                }
            }
        }
    }
}
