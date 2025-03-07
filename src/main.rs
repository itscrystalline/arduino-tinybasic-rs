#![no_std]
#![no_main]

use core::char;

use arduino_hal::prelude::*;
use arrayvec::{ArrayString, ArrayVec};
use panic_halt as _;
use ufmt::{derive::uDebug, uwrite, uwriteln};

const RETURN_ASCII: u8 = 13; //\r
const LINE_LENGTH: usize = 64;

#[derive(Copy, Clone)]
enum BasicCommand {
    Print(ArrayString<32>),
    Rem,
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum Token {
    Number(u8),
    Keyword(Keyword),
    String(ArrayString<32>),
    RelationOperator(Operator),
    Variable(char),
}
#[repr(u8)]
#[derive(Copy, Clone, Debug, uDebug)]
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
#[repr(u8)]
#[derive(Copy, Clone, Debug, uDebug)]
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

#[derive(Debug)]
enum ParseError {
    Malformed,
    TooManyTokens,
    Capacity,
}

impl Token {
    fn tokenize(str: &ArrayString<64>) -> Result<ArrayVec<Token, 16>, ParseError> {
        let mut tokens = ArrayVec::<Token, 16>::new();
        let mut number_buffer: Option<u8> = None;
        let mut string_buffer: Option<ArrayString<32>> = None;
        let mut keyword_buffer: Option<ArrayString<6>> = None;
        for ch in str.chars() {
            match ch {
                ' ' => {
                    if let Some(mut string) = string_buffer {
                        if string.try_push(ch).is_err() {
                            return Err(ParseError::Capacity);
                        }
                        _ = string_buffer.insert(string);
                    } else {
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
                            keyword_buffer = None
                        }
                    }
                }
                '"' => match string_buffer {
                    Some(string) => {
                        match tokens.try_push(Token::String(string)) {
                            Ok(_) => {}
                            Err(e) => return Err(ParseError::TooManyTokens),
                        };
                        string_buffer = None;
                    }
                    None => string_buffer = Some(ArrayString::new()),
                },
                num_ch if num_ch.is_ascii_digit() => {
                    let num = num_ch as u8 - 48;
                    match (number_buffer, string_buffer) {
                        (Some(number), None) => {
                            _ = number_buffer.insert(number * 10 + num);
                        }
                        (None, Some(mut str)) => {
                            if str.try_push(num_ch).is_err() {
                                return Err(ParseError::Capacity);
                            }
                            _ = string_buffer.insert(str);
                        }
                        (None, None) => number_buffer = Some(num),
                        _ => (),
                    }
                }
                char => match string_buffer {
                    Some(mut string) => {
                        if string.try_push(char).is_err() {
                            return Err(ParseError::Capacity);
                        }
                        _ = string_buffer.insert(string);
                    }
                    None => match keyword_buffer {
                        Some(mut keyword) => {
                            if keyword.try_push(char).is_err() {
                                return Err(ParseError::Capacity);
                            }
                            _ = keyword_buffer.insert(keyword)
                        }
                        None => {
                            let mut kw_buf = ArrayString::<6>::new();
                            if kw_buf.try_push(char).is_err() {
                                return Err(ParseError::Capacity);
                            }
                            keyword_buffer = Some(kw_buf);
                        }
                    },
                },
            }
        }
        if let Some(number) = number_buffer {
            if tokens.try_push(Token::Number(number)).is_err() {
                return Err(ParseError::Capacity);
            }
        }
        if let Some(string) = string_buffer {
            if tokens.try_push(Token::String(string)).is_err() {
                return Err(ParseError::Capacity);
            }
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
        Ok(tokens)
    }
}

#[arduino_hal::entry]
fn main() -> ! {
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let mut serial = arduino_hal::default_serial!(dp, pins, 57200);

    uwriteln!(&mut serial, "Starting TinyBASIC...\r").unwrap_infallible();
    uwriteln!(
        &mut serial,
        "program buffer size: {}\r",
        size_of::<[Option<BasicCommand>; 512]>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "input buffer size: {}\r",
        size_of::<ArrayString<64>>()
    )
    .unwrap_infallible();
    uwriteln!(
        &mut serial,
        "token buffer size: {}\r",
        size_of::<ArrayVec<Token, 16>>()
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

    let mut program: [Option<BasicCommand>; 512] = [None; 512];

    let mut input_buffer = ArrayString::<64>::new();
    loop {
        uwrite!(&mut serial, "> ").unwrap_infallible();
        loop {
            let char_u8 = serial.read_byte();
            match char_u8 {
                RETURN_ASCII => {
                    uwriteln!(&mut serial, "\r").unwrap_infallible();
                    match Token::tokenize(&input_buffer) {
                        Ok(tokens) => {
                            uwriteln!(&mut serial, "parse ok {} tokens\r", tokens.len())
                                .unwrap_infallible();
                            for token in tokens {
                                match token {
                                    Token::Number(num) => uwrite!(&mut serial, "num{} ", num),
                                    Token::String(str) => {
                                        uwrite!(&mut serial, "strlen{} ", str.len())
                                    }
                                    Token::RelationOperator(op) => {
                                        uwrite!(&mut serial, "op{:?} ", op)
                                    }
                                    Token::Keyword(kw) => {
                                        uwrite!(&mut serial, "kw{:?}", kw)
                                    }
                                    _ => uwrite!(&mut serial, "token "),
                                }
                                .unwrap_infallible();
                                uwriteln!(&mut serial, "\r").unwrap_infallible();
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
                                uwriteln!(&mut serial, "str cap full").unwrap_infallible();
                            }
                        },
                    }
                    input_buffer.clear();
                    break;
                }
                _ => {
                    let c = (char_u8 as char).to_ascii_uppercase();
                    match input_buffer.try_push(c) {
                        Ok(_) => uwrite!(&mut serial, "{}", c).unwrap_infallible(),
                        Err(e) => uwrite!(&mut serial, "cannot put chr in").unwrap_infallible(),
                    }
                }
            }
        }
    }
}
