use arduino_hal::prelude::_unwrap_infallible_UnwrapInfallible;
use arrayvec::{ArrayString, ArrayVec};
use ufmt::{derive::uDebug, uwrite, uwriteln};

use crate::Serial;

const A_ORD: u8 = b'A';

#[derive(PartialEq, Eq)]
pub enum BasicControlFlow {
    Run,
    Continue,
    End,
    List,
    Clear,
    Goto(u8),
}
pub enum InterpretationError {
    UnexpectedArgs,
    NoArgs,
}

#[derive(Clone, Copy)]
pub enum ExpectedArgument {
    Keyword,
    Number,
    String,
}
#[derive(Copy, Clone)]
pub enum MathToken {
    Literal(u8),
    Variable(u8),
}
#[derive(Clone)]
pub enum Expression {
    String(ArrayString<32>),
    Math(ArrayVec<MathToken, 10>, ArrayVec<MathOperator, 8>),
}
pub enum EvaluationError {
    Incomplete,
    Overflow,
    Underflow,
    DivideByZero,
}
impl Expression {
    fn evaluate_math(
        mut numbers: ArrayVec<MathToken, 10>,
        mut ops: ArrayVec<MathOperator, 8>,
        variables: &mut [u8],
    ) -> Result<u8, EvaluationError> {
        while let Some(op) = ops.pop() {
            let (rhs, lhs) = (
                numbers.pop().ok_or(EvaluationError::Incomplete)?,
                numbers.pop().ok_or(EvaluationError::Incomplete)?,
            );
            let rhs_num = match rhs {
                MathToken::Literal(n) => n,
                MathToken::Variable(idx) => variables[idx as usize],
            };
            let lhs_num = match lhs {
                MathToken::Literal(n) => n,
                MathToken::Variable(idx) => variables[idx as usize],
            };
            numbers.push(MathToken::Literal(match op {
                MathOperator::Plus => lhs_num
                    .checked_add(rhs_num)
                    .ok_or(EvaluationError::Overflow),
                MathOperator::Minus => lhs_num
                    .checked_sub(rhs_num)
                    .ok_or(EvaluationError::Underflow),
                MathOperator::Multiply => lhs_num
                    .checked_mul(rhs_num)
                    .ok_or(EvaluationError::Overflow),
                MathOperator::Divide => lhs_num
                    .checked_div(rhs_num)
                    .ok_or(EvaluationError::DivideByZero),
            }?));
        }
        match numbers.pop() {
            Some(MathToken::Literal(res)) => Ok(res),
            _ => Err(EvaluationError::Incomplete),
        }
    }
}

#[derive(Clone)]
pub enum BasicCommand {
    Print(Option<Expression>),
    Run,
    Goto(Option<u8>),
    End,
    List,
    Clear,
    Let(Option<u8>),
    Rem,
}
impl BasicCommand {
    pub fn execute(&self, serial: &mut Serial, variables: &mut [u8]) -> BasicControlFlow {
        match self {
            BasicCommand::Print(Some(expr)) => {
                match expr {
                    Expression::String(str) => str
                        .chars()
                        .for_each(|ch| uwrite!(serial, "{}", ch).unwrap_infallible()),
                    Expression::Math(numbers, operators) => {
                        let result = Expression::evaluate_math(
                            numbers.clone(),
                            operators.clone(),
                            variables,
                        );
                        match result {
                            Ok(res) => uwrite!(serial, "{}", res),
                            Err(e) => match e {
                                EvaluationError::Incomplete => {
                                    uwrite!(serial, "incomplete expression")
                                }
                                EvaluationError::Overflow => uwrite!(serial, "value overflowed"),
                                EvaluationError::Underflow => uwrite!(serial, "value underflowed"),
                                EvaluationError::DivideByZero => {
                                    uwrite!(serial, "division by zero")
                                }
                            },
                        }
                        .unwrap_infallible();
                    }
                }
                uwriteln!(serial, "").unwrap_infallible();
            }
            BasicCommand::Run => return BasicControlFlow::Run,
            BasicCommand::End => return BasicControlFlow::End,
            BasicCommand::Clear => return BasicControlFlow::Clear,
            BasicCommand::Goto(line) if line.is_some() => {
                return BasicControlFlow::Goto(line.unwrap())
            }
            BasicCommand::List => return BasicControlFlow::List,
            BasicCommand::Rem => (),
            _ => uwriteln!(serial, "UNIMPLEMENTED").unwrap_infallible(),
        }
        BasicControlFlow::Continue
    }
}
#[derive(Clone)]
pub struct BasicLine {
    pub line_num: Option<usize>,
    pub command: BasicCommand,
}
impl BasicLine {
    pub fn execute(self, serial: &mut Serial, variables: &mut [u8]) -> BasicControlFlow {
        self.command.execute(serial, variables)
    }

    pub fn from_tokens(tokens: ArrayVec<Token, 8>) -> Result<BasicLine, InterpretationError> {
        macro_rules! single_command {
            ($command: expr, $expected: expr, $comm: expr) => {
                $command = $comm;
                $expected = None;
            };
        }

        let mut line_num = None;
        let mut expected = Some(ExpectedArgument::Keyword);

        let mut command = BasicCommand::Rem;
        for (idx, token) in tokens.into_iter().enumerate() {
            if expected.is_none() {
                break;
            }

            if idx == 0usize {
                if let Token::Number(num) = token {
                    line_num = Some(num as usize);
                    continue;
                }
            }

            if token.is(expected.unwrap()) {
                match token {
                    Token::Keyword(kw) => match kw {
                        Keyword::Print => {
                            command = BasicCommand::Print(None);
                            expected = Some(ExpectedArgument::String);
                        }
                        Keyword::Run => {
                            single_command!(command, expected, BasicCommand::Run);
                        }
                        Keyword::End => {
                            single_command!(command, expected, BasicCommand::End);
                        }
                        Keyword::List => {
                            single_command!(command, expected, BasicCommand::List);
                        }
                        Keyword::Clear => {
                            single_command!(command, expected, BasicCommand::Clear);
                        }
                        Keyword::Goto => {
                            command = BasicCommand::Goto(None);
                            expected = Some(ExpectedArgument::Number);
                        }
                        Keyword::Let => {
                            command = BasicCommand::Let(None);
                            expected = Some(ExpectedArgument::Number);
                        }
                        _ => todo!(),
                    },
                    Token::String(str) => {
                        if let BasicCommand::Print(_) = command {
                            command = BasicCommand::Print(Some(Expression::String(str)));
                            expected = None;
                        }
                    }
                    Token::Number(num) => match command {
                        BasicCommand::Goto(_) => {
                            command = BasicCommand::Goto(Some(num));
                            expected = None;
                        }
                        BasicCommand::Let(_) => {
                            command = BasicCommand::Let(Some(num));
                            expected = None;
                        }
                        _ => (),
                    },
                    _ => todo!(),
                }
            } else {
                return Err(InterpretationError::UnexpectedArgs);
            }
        }

        match expected {
            None => Ok(BasicLine { line_num, command }),
            Some(_) => Err(InterpretationError::NoArgs),
        }
    }

    pub fn is_immeadiate(&self) -> bool {
        self.line_num.is_none()
    }
}

#[derive(Copy, Clone, Debug, uDebug)]
enum MathOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}
impl MathOperator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "+" => Ok(MathOperator::Plus),
            "-" => Ok(MathOperator::Minus),
            "*" => Ok(MathOperator::Multiply),
            "/" => Ok(MathOperator::Divide),
            _ => Err(InvalidKeywordError),
        }
    }
}

#[derive(Copy, Clone, Debug, uDebug)]
enum RelationOperator {
    Equal,     // =
    NotEqual,  // <>
    Less,      // <
    More,      // >
    LessEqual, // <=
    MoreEqual, // >=
}
impl RelationOperator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "=" => Ok(RelationOperator::Equal),
            "<>" => Ok(RelationOperator::NotEqual),
            "<" => Ok(RelationOperator::Less),
            ">" => Ok(RelationOperator::More),
            "<=" => Ok(RelationOperator::LessEqual),
            ">=" => Ok(RelationOperator::MoreEqual),
            _ => Err(InvalidKeywordError),
        }
    }
}

#[derive(Copy, Clone, Debug, uDebug)]
pub enum Keyword {
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

#[derive(Copy, Clone, Debug)]
pub enum Token {
    Number(u8),
    Keyword(Keyword),
    String(ArrayString<32>),
    RelationOperator(RelationOperator),
    MathOperator(MathOperator),
    Variable(u8),
}
pub enum ParseError {
    Malformed,
    TooManyTokens,
    Capacity,
    NumberOverflow,
}

impl Token {
    pub fn tokenize(str: &ArrayString<64>) -> Result<ArrayVec<Token, 8>, ParseError> {
        let mut tokens = ArrayVec::<Token, 8>::new();
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
                                Err(_) => match RelationOperator::from(keyword) {
                                    Err(_) => match MathOperator::from(keyword) {
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
                '"' | '\'' => match string_buffer {
                    Some(string) => {
                        if tokens.try_push(Token::String(string)).is_err() {
                            return Err(ParseError::TooManyTokens);
                        };
                        string_buffer = None;
                    }
                    None => string_buffer = Some(ArrayString::new()),
                },
                num_ch if num_ch.is_ascii_digit() => {
                    let num = num_ch as u8 - b'0';
                    match (number_buffer, string_buffer) {
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
                Err(_) => match RelationOperator::from(keyword) {
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

    fn is(&self, expected: ExpectedArgument) -> bool {
        matches!(
            (self, expected),
            (Token::Keyword(_), ExpectedArgument::Keyword)
                | (Token::String(_), ExpectedArgument::String)
                | (Token::Number(_), ExpectedArgument::Number)
        )
    }
}
