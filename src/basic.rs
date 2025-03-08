use arduino_hal::prelude::*;
use arrayvec::{ArrayString, ArrayVec};
use ufmt::{derive::uDebug, uDisplay, uwrite, uwriteln};

use crate::Serial;

#[derive(PartialEq, Eq)]
pub enum BasicControlFlow {
    Run,
    Continue,
    End,
    List,
    Clear,
    Goto(usize),
}
pub enum InterpretationError {
    UnexpectedArgs,
    NoArgs,
    UnimplementedToken,
    StackFull,
}

#[derive(Clone, Copy)]
pub enum ExpectedArgument {
    Keyword,
    Expression,
    Number,
    //String,
    Variable,
    Assignment,
}
#[derive(Copy, Clone)]
pub enum MathToken {
    Operator(MathOperator),
    Variable(u8),
    Literal(usize),
}
#[derive(Clone)]
pub enum Expression {
    String(ArrayString<32>),
    Math(ArrayVec<MathToken, 7>),
    Boolean(
        RelationOperator,
        ArrayVec<MathToken, 3>,
        ArrayVec<MathToken, 3>,
    ),
}
pub enum EvaluationError {
    Incomplete,
    StackFull,
    Overflow,
    Underflow,
    DivideByZero,
}
impl Expression {
    fn evaluate_math<const N: usize>(
        mut tokens: ArrayVec<MathToken, N>,
        variables: &mut [usize],
    ) -> Result<usize, EvaluationError> {
        let mut stack = ArrayVec::<usize, 2>::new();

        while let Some(token) = tokens.pop() {
            match token {
                MathToken::Operator(op) => {
                    let (rhs, lhs) = (
                        stack.pop().ok_or(EvaluationError::Incomplete)?,
                        stack.pop().ok_or(EvaluationError::Incomplete)?,
                    );
                    stack
                        .try_push(match op {
                            MathOperator::Plus => {
                                lhs.checked_add(rhs).ok_or(EvaluationError::Overflow)?
                            }
                            MathOperator::Minus => {
                                lhs.checked_sub(rhs).ok_or(EvaluationError::Underflow)?
                            }
                            MathOperator::Multiply => {
                                lhs.checked_mul(rhs).ok_or(EvaluationError::Overflow)?
                            }
                            MathOperator::Divide => {
                                lhs.checked_div(rhs).ok_or(EvaluationError::DivideByZero)?
                            }
                        })
                        .map_err(|_| EvaluationError::StackFull)?;
                }
                MathToken::Literal(num) => stack
                    .try_push(num)
                    .map_err(|_| EvaluationError::StackFull)?,
                MathToken::Variable(var_idx) => stack
                    .try_push(variables[var_idx as usize])
                    .map_err(|_| EvaluationError::StackFull)?,
            }
        }

        stack.pop().ok_or(EvaluationError::Incomplete)
    }

    fn evaluate_boolean(
        rel: &RelationOperator,
        left_tokens: ArrayVec<MathToken, 3>,
        right_tokens: ArrayVec<MathToken, 3>,
        variables: &mut [usize],
    ) -> Result<bool, EvaluationError> {
        let left = Self::evaluate_math(left_tokens, variables)?;
        let right = Self::evaluate_math(right_tokens, variables)?;

        Ok(rel.compare(left, right))
    }
}

#[derive(Clone)]
pub enum BasicCommand {
    Print(Option<Expression>),
    Run,
    Goto(Option<usize>),
    End,
    List,
    Clear,
    Let(Option<u8>, Option<Expression>),
    Rem,
}
impl BasicCommand {
    pub fn execute(&self, serial: &mut Serial, variables: &mut [usize]) -> BasicControlFlow {
        match self {
            BasicCommand::Print(Some(expr)) => {
                match expr {
                    Expression::String(str) => str
                        .chars()
                        .for_each(|ch| uwrite!(serial, "{}", ch).unwrap_infallible()),
                    Expression::Math(numbers) => {
                        let result = Expression::evaluate_math(numbers.clone(), variables);
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
                                EvaluationError::StackFull => {
                                    uwrite!(serial, "number stack full")
                                }
                            },
                        }
                        .unwrap_infallible();
                    }
                    Expression::Boolean(cmp, left, right) => {
                        let res = Expression::evaluate_boolean(
                            cmp,
                            left.clone(),
                            right.clone(),
                            variables,
                        );
                        match res {
                            Ok(val) => uwrite!(serial, "{}", val).unwrap_infallible(),
                            Err(e) => match e {
                                EvaluationError::Incomplete => {
                                    uwrite!(serial, "incomplete expression")
                                }
                                EvaluationError::Overflow => uwrite!(serial, "value overflowed"),
                                EvaluationError::Underflow => uwrite!(serial, "value underflowed"),
                                EvaluationError::DivideByZero => {
                                    uwrite!(serial, "division by zero")
                                }
                                EvaluationError::StackFull => {
                                    uwrite!(serial, "number stack full")
                                }
                            }
                            .unwrap_infallible(),
                        }
                    }
                }
                uwriteln!(serial, "\r").unwrap_infallible();
            }
            BasicCommand::Run => return BasicControlFlow::Run,
            BasicCommand::End => return BasicControlFlow::End,
            BasicCommand::Clear => return BasicControlFlow::Clear,
            BasicCommand::Goto(line) if line.is_some() => {
                return BasicControlFlow::Goto(line.unwrap())
            }
            BasicCommand::List => return BasicControlFlow::List,
            BasicCommand::Let(Some(var_idx), Some(Expression::Math(numbers))) => {
                let result = Expression::evaluate_math(numbers.clone(), variables);
                match result {
                    Ok(res) => variables[*var_idx as usize] = res,
                    Err(e) => {
                        match e {
                            EvaluationError::Incomplete => {
                                uwrite!(serial, "incomplete expression").unwrap_infallible()
                            }
                            EvaluationError::Overflow => {
                                uwrite!(serial, "value overflowed").unwrap_infallible()
                            }
                            EvaluationError::Underflow => {
                                uwrite!(serial, "value underflowed").unwrap_infallible()
                            }
                            EvaluationError::DivideByZero => {
                                uwrite!(serial, "division by zero").unwrap_infallible()
                            }
                            EvaluationError::StackFull => {
                                uwrite!(serial, "number stack full").unwrap_infallible()
                            }
                        };
                        uwriteln!(serial, "\r").unwrap_infallible();
                        return BasicControlFlow::End;
                    }
                };
            }
            BasicCommand::Rem => (),
            _ => uwriteln!(serial, "UNIMPLEMENTED\r").unwrap_infallible(),
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
    pub fn execute(self, serial: &mut Serial, variables: &mut [usize]) -> BasicControlFlow {
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
        let mut operator_stack = ArrayVec::<MathOperator, 3>::new();

        let mut command = BasicCommand::Rem;
        for (idx, token) in tokens.into_iter().enumerate() {
            if expected.is_none() {
                break;
            }

            if idx == 0usize {
                if let Token::Number(num) = token {
                    line_num = Some(num);
                    continue;
                }
            }

            if token.is(expected.unwrap()) {
                match token {
                    Token::Keyword(kw) => match kw {
                        Keyword::Print => {
                            command = BasicCommand::Print(None);
                            expected = Some(ExpectedArgument::Expression);
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
                            command = BasicCommand::Let(None, None);
                            expected = Some(ExpectedArgument::Variable);
                        }
                        _ => return Err(InterpretationError::UnimplementedToken),
                    },
                    Token::String(str) => match command {
                        BasicCommand::Print(None) => {
                            command = BasicCommand::Print(Some(Expression::String(str)));
                            expected = None;
                        }
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                    Token::Number(num) => match command {
                        BasicCommand::Goto(_) => {
                            command = BasicCommand::Goto(Some(num));
                            expected = None;
                        }
                        BasicCommand::Let(Some(_), ref mut expr) => match expr {
                            None => {
                                let mut tokens = ArrayVec::<MathToken, 7>::new();
                                tokens.push(MathToken::Literal(num));
                                *expr = Some(Expression::Math(tokens));
                            }
                            Some(Expression::Math(ref mut nums)) => {
                                nums.try_push(MathToken::Literal(num))
                                    .map_err(|_| InterpretationError::StackFull)?;
                            }
                            _ => return Err(InterpretationError::UnexpectedArgs),
                        },
                        BasicCommand::Print(ref mut expr) => match expr {
                            None => {
                                let mut tokens = ArrayVec::<MathToken, 7>::new();
                                tokens.push(MathToken::Literal(num));
                                *expr = Some(Expression::Math(tokens));
                            }
                            Some(Expression::Math(ref mut nums)) => {
                                nums.try_push(MathToken::Literal(num))
                                    .map_err(|_| InterpretationError::StackFull)?;
                            }
                            _ => return Err(InterpretationError::UnexpectedArgs),
                        },
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                    Token::Variable(var_idx) => match command {
                        BasicCommand::Print(Some(Expression::Math(ref mut tokens)))
                        | BasicCommand::Let(Some(_), Some(Expression::Math(ref mut tokens))) => {
                            tokens
                                .try_push(MathToken::Variable(var_idx))
                                .map_err(|_| InterpretationError::StackFull)?;
                        }
                        BasicCommand::Print(ref mut expr) if expr.is_none() => {
                            let mut numbers = ArrayVec::<MathToken, 7>::new();
                            numbers.push(MathToken::Variable(var_idx));
                            *expr = Some(Expression::Math(numbers));
                        }
                        BasicCommand::Let(ref mut var, None) if var.is_none() => {
                            *var = Some(var_idx);
                            expected = Some(ExpectedArgument::Assignment);
                        }
                        BasicCommand::Let(Some(_), ref mut expr) if expr.is_none() => {
                            let mut numbers = ArrayVec::<MathToken, 7>::new();
                            numbers.push(MathToken::Variable(var_idx));
                            *expr = Some(Expression::Math(numbers));
                        }
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                    Token::MathOperator(op) => match command {
                        BasicCommand::Print(Some(Expression::Math(ref mut tokens)))
                        | BasicCommand::Let(Some(_), Some(Expression::Math(ref mut tokens))) => {
                            match operator_stack.pop() {
                                None => operator_stack
                                    .try_push(op)
                                    .map_err(|_| InterpretationError::StackFull)?,
                                // if this operator preceedes the existing one, add it to the
                                // operator stack after pushing the existing one back in
                                Some(top) if op.preceedes(top) => {
                                    operator_stack
                                        .try_push(top)
                                        .map_err(|_| InterpretationError::StackFull)?;
                                    operator_stack
                                        .try_push(op)
                                        .map_err(|_| InterpretationError::StackFull)?;
                                }
                                // else, pop the one from the opstack onto the token stack and push
                                // this operator onto the opstack
                                Some(top) => {
                                    tokens
                                        .try_push(MathToken::Operator(top))
                                        .map_err(|_| InterpretationError::StackFull)?;
                                    operator_stack
                                        .try_push(op)
                                        .map_err(|_| InterpretationError::StackFull)?;
                                }
                            }
                        }
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                    Token::RelationOperator(op) => match op {
                        RelationOperator::Equal => match command {
                            BasicCommand::Let(Some(_), None) => {
                                expected = Some(ExpectedArgument::Expression)
                            }
                            _ => return Err(InterpretationError::UnexpectedArgs),
                        },
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                }
            } else {
                return Err(InterpretationError::UnexpectedArgs);
            }
        }

        match command {
            BasicCommand::Print(Some(ref mut expr))
            | BasicCommand::Let(Some(_), Some(ref mut expr)) => {
                expected = None;
                if let Expression::Math(ref mut tokens) = expr {
                    while let Some(op) = operator_stack.pop() {
                        tokens
                            .try_push(MathToken::Operator(op))
                            .map_err(|_| InterpretationError::StackFull)?;
                    }
                    tokens.reverse();
                }
            }
            _ => (),
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
pub enum MathOperator {
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
    fn preceedes(self, other: Self) -> bool {
        matches!(
            (self, other),
            (Self::Multiply | Self::Divide, Self::Plus | Self::Minus)
        )
    }
}

#[derive(Copy, Clone, Debug, uDebug)]
pub enum RelationOperator {
    Equal,     // =
    NotEqual,  // <>
    Less,      // <
    More,      // >
    LessEqual, // <=
    MoreEqual, // >=
}
impl uDisplay for RelationOperator {
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
impl RelationOperator {
    fn from(buf: ArrayString<6>) -> Result<Self, InvalidKeywordError> {
        match buf.as_str() {
            "=" => Ok(RelationOperator::Equal),
            "<>" => Ok(RelationOperator::NotEqual),
            "<" => Ok(RelationOperator::Less),
            "<=" => Ok(RelationOperator::LessEqual),
            ">" => Ok(RelationOperator::More),
            ">=" => Ok(RelationOperator::MoreEqual),
            _ => Err(InvalidKeywordError),
        }
    }
    fn compare(&self, left: usize, right: usize) -> bool {
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
    Number(usize),
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
        let mut number_buffer: Option<usize> = None;
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
                    let num = (num_ch as u8 - b'0') as usize;
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
                    Err(_) => match MathOperator::from(keyword) {
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
                | (Token::String(_), ExpectedArgument::Expression)
                | (Token::Number(_), ExpectedArgument::Number)
                | (Token::Number(_), ExpectedArgument::Expression)
                | (Token::Variable(_), ExpectedArgument::Variable)
                | (Token::Variable(_), ExpectedArgument::Expression)
                | (Token::MathOperator(_), ExpectedArgument::Expression)
                | (
                    Token::RelationOperator(RelationOperator::Equal),
                    ExpectedArgument::Assignment
                )
        )
    }
}
