use arduino_hal::prelude::*;

use arrayvec::{ArrayString, ArrayVec};
use ufmt::{derive::uDebug, uDisplay};

#[derive(Clone, Copy)]
pub enum ExpectedArgument {
    Keyword,
    Expression,
    NumExpressionOrThen,
    Number,
    Variable,
    Assignment,
}

#[derive(Copy, Clone, Debug, uDebug)]
pub enum MathOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    Modulo,
}
impl MathOperator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "+" => Ok(MathOperator::Plus),
            "-" => Ok(MathOperator::Minus),
            "*" => Ok(MathOperator::Multiply),
            "/" => Ok(MathOperator::Divide),
            "^" => Ok(MathOperator::Power),
            "%" => Ok(MathOperator::Modulo),
            _ => Err(InvalidKeywordError),
        }
    }
    pub fn preceedes(self, other: Self) -> bool {
        matches!(
            (self, other),
            (Self::Multiply | Self::Divide, Self::Plus | Self::Minus)
                | (Self::Power, Self::Plus | Self::Minus)
                | (Self::Power, Self::Multiply | Self::Divide)
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, uDebug)]
pub enum ComparisionOperator {
    Equal,     // =
    NotEqual,  // <>
    Less,      // <
    More,      // >
    LessEqual, // <=
    MoreEqual, // >=
}
impl uDisplay for ComparisionOperator {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: _ufmt_uWrite + ?Sized,
    {
        f.write_str(match self {
            Self::Equal => "=",
            Self::NotEqual => "<>",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::More => ">",
            Self::MoreEqual => ">=",
        })
    }
}
impl ComparisionOperator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "=" => Ok(ComparisionOperator::Equal),
            "<>" => Ok(ComparisionOperator::NotEqual),
            "<" => Ok(ComparisionOperator::Less),
            "<=" => Ok(ComparisionOperator::LessEqual),
            ">" => Ok(ComparisionOperator::More),
            ">=" => Ok(ComparisionOperator::MoreEqual),
            _ => Err(InvalidKeywordError),
        }
    }
    pub fn compare(&self, left: usize, right: usize) -> bool {
        match self {
            Self::Equal => left == right,
            Self::NotEqual => left != right,
            Self::Less => left < right,
            Self::LessEqual => left <= right,
            Self::More => left > right,
            Self::MoreEqual => left >= right,
        }
    }
}

#[derive(Copy, Clone, Debug, uDebug)]
pub enum Keyword {
    Print,
    If,
    Then,
    Goto,
    Input,
    Let,
    Clear,
    List,
    Run,
    End,

    AnalogRead,
    DigitalRead,
    DigitalWrite,
}
struct InvalidKeywordError;

impl Keyword {
    fn from(value: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match value.as_str() {
            "PRINT" => Ok(Keyword::Print),
            "IF" => Ok(Keyword::If),
            "THEN" => Ok(Keyword::Then),
            "GOTO" => Ok(Keyword::Goto),
            "INPUT" => Ok(Keyword::Input),
            "LET" => Ok(Keyword::Let),
            "CLEAR" => Ok(Keyword::Clear),
            "LIST" => Ok(Keyword::List),
            "RUN" => Ok(Keyword::Run),
            "END" => Ok(Keyword::End),

            "AREAD" => Ok(Keyword::AnalogRead),
            "DREAD" => Ok(Keyword::DigitalRead),
            "DWRITE" => Ok(Keyword::DigitalWrite),

            _ => Err(InvalidKeywordError),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Token {
    Number(usize),
    Keyword(Keyword),
    String(String),
    ComparisionOperator(ComparisionOperator),
    MathOperator(MathOperator),
    Variable(u8),
}
pub enum ParseError {
    Malformed,
    TooManyTokens,
    Capacity,
    NumberOverflow,
}

pub type TokenBuffer = ArrayVec<Token, 11>;
pub type String = ArrayString<16>;
pub type KeywordBuffer = Option<ArrayString<6>>;

impl Token {
    pub fn tokenize(
        str: &ArrayString<32>,
        tokens: &mut TokenBuffer,
        number_buffer: &mut Option<usize>,
        string_buffer: &mut Option<String>,
        keyword_buffer: &mut KeywordBuffer,
    ) -> Result<(), ParseError> {
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
                            tokens
                                .try_push(Token::Number(*number))
                                .map_err(|_| ParseError::TooManyTokens)?;
                            *number_buffer = None;
                        }
                        if let Some(keyword) = keyword_buffer {
                            match Keyword::from(*keyword) {
                                Err(_) => match ComparisionOperator::from(*keyword) {
                                    Err(_) => match MathOperator::from(*keyword) {
                                        Err(_) => {
                                            if keyword.len() == 1 {
                                                let var_ord = keyword
                                                    .chars()
                                                    .next()
                                                    .map_or(b'A', |ch| ch as u8);
                                                tokens
                                                    .try_push(Token::Variable(var_ord - b'A'))
                                                    .map_err(|_| ParseError::TooManyTokens)?;
                                            } else {
                                                return Err(ParseError::Malformed);
                                            }
                                        }
                                        Ok(op) => tokens
                                            .try_push(Token::MathOperator(op))
                                            .map_err(|_| ParseError::TooManyTokens)?,
                                    },
                                    Ok(op) => tokens
                                        .try_push(Token::ComparisionOperator(op))
                                        .map_err(|_| ParseError::TooManyTokens)?,
                                },
                                Ok(kw) => tokens
                                    .try_push(Token::Keyword(kw))
                                    .map_err(|_| ParseError::TooManyTokens)?,
                            }
                            *keyword_buffer = None
                        }
                    }
                }
                '"' | '\'' => match string_buffer {
                    Some(string) => {
                        if tokens.try_push(Token::String(*string)).is_err() {
                            return Err(ParseError::TooManyTokens);
                        };
                        *string_buffer = None;
                    }
                    None => *string_buffer = Some(ArrayString::new()),
                },
                num_ch if num_ch.is_ascii_digit() => {
                    let num = (num_ch as u8 - b'0') as usize;
                    match (&number_buffer, &string_buffer) {
                        (Some(number), None) => {
                            let new_number = number
                                .checked_mul(10)
                                .ok_or(ParseError::NumberOverflow)?
                                .checked_add(num)
                                .ok_or(ParseError::NumberOverflow)?;
                            _ = number_buffer.insert(new_number);
                        }
                        (None, Some(mut str)) => {
                            if str.try_push(num_ch).is_err() {
                                return Err(ParseError::Capacity);
                            }
                            _ = string_buffer.insert(str);
                        }
                        (None, None) => *number_buffer = Some(num),
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
                            *keyword_buffer = Some(kw_buf);
                        }
                    },
                },
            }
        }
        if let Some(number) = number_buffer {
            if tokens.try_push(Token::Number(*number)).is_err() {
                return Err(ParseError::Capacity);
            }
        }
        if let Some(string) = string_buffer {
            if tokens.try_push(Token::String(*string)).is_err() {
                return Err(ParseError::Capacity);
            }
        }
        if let Some(keyword) = keyword_buffer {
            match Keyword::from(*keyword) {
                Err(_) => match ComparisionOperator::from(*keyword) {
                    Err(_) => match MathOperator::from(*keyword) {
                        Err(_) => {
                            if keyword.len() == 1 {
                                let var_ord = keyword.chars().next().map_or(b'A', |ch| ch as u8);
                                tokens
                                    .try_push(Token::Variable(var_ord - b'A'))
                                    .map_err(|_| ParseError::TooManyTokens)?;
                            } else {
                                return Err(ParseError::Malformed);
                            }
                        }
                        Ok(op) => tokens
                            .try_push(Token::MathOperator(op))
                            .map_err(|_| ParseError::TooManyTokens)?,
                    },
                    Ok(op) => tokens
                        .try_push(Token::ComparisionOperator(op))
                        .map_err(|_| ParseError::TooManyTokens)?,
                },
                Ok(kw) => tokens
                    .try_push(Token::Keyword(kw))
                    .map_err(|_| ParseError::TooManyTokens)?,
            }
        }
        Ok(())
    }

    pub fn is(&self, expected: ExpectedArgument) -> bool {
        matches!(
            (self, expected),
            (Token::Keyword(_), ExpectedArgument::Keyword)
                | (
                    Token::Keyword(Keyword::Then),
                    ExpectedArgument::NumExpressionOrThen
                )
                | (Token::String(_), ExpectedArgument::Expression)
                | (Token::Number(_), ExpectedArgument::Number)
                | (Token::Number(_), ExpectedArgument::Expression)
                | (Token::Number(_), ExpectedArgument::NumExpressionOrThen)
                | (Token::Variable(_), ExpectedArgument::Variable)
                | (Token::Variable(_), ExpectedArgument::Expression)
                | (Token::Variable(_), ExpectedArgument::NumExpressionOrThen)
                | (Token::MathOperator(_), ExpectedArgument::Expression)
                | (
                    Token::MathOperator(_),
                    ExpectedArgument::NumExpressionOrThen
                )
                | (Token::ComparisionOperator(_), ExpectedArgument::Expression)
                | (
                    Token::ComparisionOperator(_),
                    ExpectedArgument::NumExpressionOrThen
                )
                | (
                    Token::ComparisionOperator(ComparisionOperator::Equal),
                    ExpectedArgument::Assignment
                )
        )
    }
}
