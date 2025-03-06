#![no_std]
#![no_main]

use core::char;

use arduino_hal::prelude::*;
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
    Equal,     // =
    NotEqual,  // <>
    Less,      // <
    More,      // >
    LessEqual, // <=
    MoreEqual, // >=
}
impl Operator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "=" => Ok(Operator::Equal),
            "<>" => Ok(Operator::NotEqual),
            "<" => Ok(Operator::Less),
            ">" => Ok(Operator::More),
            "<=" => Ok(Operator::LessEqual),
            ">=" => Ok(Operator::MoreEqual),
            _ => Err(InvalidKeywordError),
        }
    }
}

enum Keyword {
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
struct InvalidKeywordError;

impl Keyword {
    fn from(value: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match value.as_str() {
            "PRINT" => Ok(Keyword::Print),
            "IF" => Ok(Keyword::If),
            "GOTO" => Ok(Keyword::Goto),
            "INPUT" => Ok(Keyword::Input),
            "LET" => Ok(Keyword::Let),
            "GOSUB" => Ok(Keyword::GoSub),
            "RETURN" => Ok(Keyword::Return),
            "CLEAR" => Ok(Keyword::Clear),
            "LIST" => Ok(Keyword::List),
            "RUN" => Ok(Keyword::Run),
            "END" => Ok(Keyword::End),
            _ => Err(InvalidKeywordError),
        }
    }
}

enum ParseError {
    Malformed,
    TooManyTokens,
}

impl Token {
    fn tokenize(str: ArrayString<64>) -> Result<ArrayVec<Token, 64>, ParseError> {
        let mut tokens = ArrayVec::<Token, 64>::new();
        let mut number_buffer: Option<u8> = None;
        let mut string_buffer: Option<ArrayString<32>> = None;
        let mut keyword_buffer: Option<ArrayString<6>> = None;
        for ch in str.chars() {
            match ch {
                ' ' => {
                    if let Some(number) = number_buffer {
                        tokens.push(Token::Number(number));
                        number_buffer = None;
                    }
                    if let Some(keyword) = keyword_buffer {
                        match Keyword::from(keyword) {
                            Err(_) => match Operator::from(keyword) {
                                Err(_) => return Err(ParseError::Malformed),
                                Ok(op) => tokens
                                    .try_push(Token::RelationOperator(op))
                                    .map_err(|_| ParseError::TooManyTokens)?,
                            },
                            Ok(kw) => tokens
                                .try_push(Token::Keyword(kw))
                                .map_err(|_| ParseError::TooManyTokens)?,
                        }
                    }
                }
                '"' => match string_buffer {
                    Some(string) => {
                        tokens.push(Token::String(string));
                        string_buffer = None;
                    }
                    None => string_buffer = Some(ArrayString::new()),
                },
                num_ch if num_ch.is_ascii_digit() => {
                    let num = num_ch as u8 - 48;
                    match (number_buffer, string_buffer) {
                        (Some(ref mut number), None) => *number = *number * 10 + num,
                        (None, Some(ref mut str)) => str.push(num_ch),
                        (None, None) => number_buffer = Some(num),
                        _ => (),
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
        let mut input_buffer = ArrayString::<64>::new();
        uwrite!(&mut serial, "> ").unwrap_infallible();
        loop {
            let char_u8 = serial.read_byte();
            match char_u8 {
                RETURN_ASCII => {
                    // TODO: figure out why this resets the board
                    //let _tokens = Token::tokenize(input_buffer);
                    uwriteln!(&mut serial, "").unwrap_infallible();
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
