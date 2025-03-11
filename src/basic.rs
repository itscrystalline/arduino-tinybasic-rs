use core::char;

use arduino_hal::prelude::*;
use arrayvec::{ArrayString, ArrayVec};
use ufmt::{derive::uDebug, uDebug, uDisplay, uwrite, uwriteln};

use crate::{
    arduino::{PinError, Pins},
    put_string_table, Serial, ERROR_TABLE, PROGRAM_LENGTH,
};

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
    StringTableFull,
    MathToBooleanFailed,
}

#[derive(Clone, Copy)]
pub enum ExpectedArgument {
    Keyword,
    Expression,
    NumExpressionOrThen,
    Number,
    Variable,
    Assignment,
}
#[derive(Copy, Clone)]
pub enum MathToken {
    Operator(MathOperator),
    Variable(u8),
    Literal(usize),
}
impl uDisplay for MathToken {
    fn fmt<W>(&self, f: &mut ufmt::Formatter<'_, W>) -> Result<(), W::Error>
    where
        W: _ufmt_uWrite + ?Sized,
    {
        match self {
            Self::Operator(op) => uDebug::fmt(op, f),
            Self::Literal(num) => uDisplay::fmt(num, f),
            Self::Variable(var_idx) => f.write_char((var_idx + b'A') as char),
        }
    }
}
#[derive(Clone)]
pub enum Expression {
    String(u8),
    Math(ArrayVec<MathToken, 5>),
    Boolean(
        Option<MathToken>,
        Option<ComparisionOperator>,
        Option<MathToken>,
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
                            MathOperator::Modulo => lhs % rhs,
                            MathOperator::Power => lhs
                                .checked_pow(rhs as u32)
                                .ok_or(EvaluationError::Overflow)?,
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
        cmp: &ComparisionOperator,
        left_token: MathToken,
        right_token: MathToken,
        variables: &mut [usize],
    ) -> Result<bool, EvaluationError> {
        let left = Self::evaluate_math(ArrayVec::<MathToken, 1>::from([left_token]), variables)?;
        let right = Self::evaluate_math(ArrayVec::<MathToken, 1>::from([right_token]), variables)?;

        Ok(cmp.compare(left, right))
    }
}

#[derive(Clone)]
pub enum BasicCommand {
    AnalogRead(Option<u8>),
    DigitalRead(Option<u8>),
    DigitalWrite(Option<u8>, Option<bool>),
    Goto(Option<usize>),
    Print(Option<Expression>),
    If(Expression, Option<usize>),
    Let(Option<u8>, Option<Expression>),
    Run,
    End,
    List,
    Clear,
    Rem,
}
impl BasicCommand {
    pub fn execute(
        &self,
        serial: &mut Serial,
        pins: &mut Pins,
        variables: &mut [usize],
        string_table: &[Option<String>],
    ) -> BasicControlFlow {
        match self {
            BasicCommand::Print(Some(expr)) => {
                match expr {
                    Expression::String(table_idx) => string_table[*table_idx as usize]
                        .unwrap()
                        .chars()
                        .for_each(|ch| uwrite!(serial, "{}", ch).unwrap_infallible()),
                    Expression::Math(numbers) => {
                        let result = Expression::evaluate_math(numbers.clone(), variables);
                        match result {
                            Ok(res) => uwrite!(serial, "{}", res),
                            Err(e) => match e {
                                EvaluationError::Incomplete => {
                                    uwrite!(serial, "{}", ERROR_TABLE[12])
                                }
                                EvaluationError::Overflow => uwrite!(serial, "{}", ERROR_TABLE[13]),
                                EvaluationError::Underflow => {
                                    uwrite!(serial, "{}", ERROR_TABLE[14])
                                }
                                EvaluationError::DivideByZero => {
                                    uwrite!(serial, "{}", ERROR_TABLE[15])
                                }
                                EvaluationError::StackFull => {
                                    uwrite!(serial, "{}", ERROR_TABLE[16])
                                }
                            },
                        }
                        .unwrap_infallible();
                    }
                    Expression::Boolean(Some(left), Some(cmp), Some(right)) => {
                        let res = Expression::evaluate_boolean(cmp, *left, *right, variables);
                        match res {
                            Ok(val) => uwrite!(serial, "{}", val).unwrap_infallible(),
                            Err(e) => match e {
                                EvaluationError::Incomplete => {
                                    uwrite!(serial, "{}", ERROR_TABLE[12])
                                }
                                EvaluationError::Overflow => uwrite!(serial, "{}", ERROR_TABLE[13]),
                                EvaluationError::Underflow => {
                                    uwrite!(serial, "{}", ERROR_TABLE[14])
                                }
                                EvaluationError::DivideByZero => {
                                    uwrite!(serial, "{}", ERROR_TABLE[15])
                                }
                                EvaluationError::StackFull => {
                                    uwrite!(serial, "{}", ERROR_TABLE[16])
                                }
                            }
                            .unwrap_infallible(),
                        }
                    }
                    _ => uwrite!(serial, "{}", ERROR_TABLE[12]).unwrap_infallible(),
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
                                uwrite!(serial, "{}", ERROR_TABLE[12]).unwrap_infallible()
                            }
                            EvaluationError::Overflow => {
                                uwrite!(serial, "{}", ERROR_TABLE[13]).unwrap_infallible()
                            }
                            EvaluationError::Underflow => {
                                uwrite!(serial, "{}", ERROR_TABLE[14]).unwrap_infallible()
                            }
                            EvaluationError::DivideByZero => {
                                uwrite!(serial, "{}", ERROR_TABLE[15]).unwrap_infallible()
                            }
                            EvaluationError::StackFull => {
                                uwrite!(serial, "{}", ERROR_TABLE[16]).unwrap_infallible()
                            }
                        };
                        uwriteln!(serial, "\r").unwrap_infallible();
                        return BasicControlFlow::End;
                    }
                };
            }
            BasicCommand::If(
                Expression::Boolean(Some(lhs), Some(cmp), Some(rhs)),
                Some(goto_dest),
            ) => {
                let res = Expression::evaluate_boolean(cmp, *lhs, *rhs, variables);
                match res {
                    Ok(val) => {
                        if val {
                            return BasicControlFlow::Goto(*goto_dest);
                        }
                    }
                    Err(e) => match e {
                        EvaluationError::Incomplete => {
                            uwrite!(serial, "{}", ERROR_TABLE[12])
                        }
                        EvaluationError::Overflow => uwrite!(serial, "{}", ERROR_TABLE[13]),
                        EvaluationError::Underflow => uwrite!(serial, "{}", ERROR_TABLE[14]),
                        EvaluationError::DivideByZero => {
                            uwrite!(serial, "{}", ERROR_TABLE[15])
                        }
                        EvaluationError::StackFull => {
                            uwrite!(serial, "{}", ERROR_TABLE[16])
                        }
                    }
                    .unwrap_infallible(),
                }
            }
            BasicCommand::AnalogRead(Some(pin)) => match pins.read_analog(*pin as usize) {
                Ok(read) => {
                    uwrite!(serial, "{}", read)
                }
                Err(e) => match e {
                    PinError::NotInput => uwrite!(serial, "{}", ERROR_TABLE[18]),
                    PinError::NotOutput => uwrite!(serial, "{}", ERROR_TABLE[19]),
                    PinError::NotPwm => uwrite!(serial, "{}", ERROR_TABLE[20]),
                    PinError::NonAddressable => uwrite!(serial, "{}", ERROR_TABLE[21]),
                    PinError::Reserved => uwrite!(serial, "{}", ERROR_TABLE[22]),
                },
            }
            .unwrap_infallible(),
            BasicCommand::DigitalRead(Some(pin)) => match pins.read_digital(*pin as usize) {
                Ok(read) => {
                    uwrite!(serial, "{}", read)
                }
                Err(e) => match e {
                    PinError::NotInput => uwrite!(serial, "{}", ERROR_TABLE[18]),
                    PinError::NotOutput => uwrite!(serial, "{}", ERROR_TABLE[19]),
                    PinError::NotPwm => uwrite!(serial, "{}", ERROR_TABLE[20]),
                    PinError::NonAddressable => uwrite!(serial, "{}", ERROR_TABLE[21]),
                    PinError::Reserved => uwrite!(serial, "{}", ERROR_TABLE[22]),
                },
            }
            .unwrap_infallible(),
            BasicCommand::DigitalWrite(Some(pin), Some(value)) => {
                if let Err(e) = pins.write_digital(*pin as usize, *value) {
                    match e {
                        PinError::NotInput => uwrite!(serial, "{}", ERROR_TABLE[18]),
                        PinError::NotOutput => uwrite!(serial, "{}", ERROR_TABLE[19]),
                        PinError::NotPwm => uwrite!(serial, "{}", ERROR_TABLE[20]),
                        PinError::NonAddressable => uwrite!(serial, "{}", ERROR_TABLE[21]),
                        PinError::Reserved => uwrite!(serial, "{}", ERROR_TABLE[22]),
                    }
                    .unwrap_infallible();
                }
            }
            BasicCommand::Rem => (),
            _ => uwriteln!(serial, "{}\r", ERROR_TABLE[17]).unwrap_infallible(),
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
    pub fn execute(
        self,
        serial: &mut Serial,
        pins: &mut Pins,
        variables: &mut [usize],
        string_table: &[Option<String>],
    ) -> BasicControlFlow {
        self.command.execute(serial, pins, variables, string_table)
    }

    pub fn from_tokens(
        tokens: &mut TokenBuffer,
        program: &mut [Option<BasicCommand>; PROGRAM_LENGTH],
        string_table: &mut [Option<String>],
    ) -> Result<BasicLine, InterpretationError> {
        macro_rules! single_command {
            ($command: expr, $expected: expr, $comm: expr) => {
                $command = $comm;
                $expected = None;
            };
        }

        let mut line_num = None;
        let mut expected = Some(ExpectedArgument::Keyword);
        let mut operator_stack = ArrayVec::<MathOperator, 2>::new();

        let mut existing_string_idx = None;

        let mut command = BasicCommand::Rem;
        for (idx, token) in tokens.drain(..).enumerate() {
            if expected.is_none() {
                break;
            }

            if idx == 0usize {
                if let Token::Number(num) = token {
                    line_num = Some(num);
                    if let Some(BasicCommand::Print(Some(Expression::String(existing_idx)))) =
                        program[num]
                    {
                        existing_string_idx = Some(existing_idx);
                    }
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
                        Keyword::If => {
                            command = BasicCommand::If(Expression::Boolean(None, None, None), None);
                            expected = Some(ExpectedArgument::NumExpressionOrThen);
                        }
                        Keyword::Then => {
                            if let BasicCommand::If(
                                Expression::Boolean(Some(_), Some(_), Some(_)),
                                None,
                            ) = command
                            {
                                expected = Some(ExpectedArgument::Number);
                            } else {
                                return Err(InterpretationError::UnexpectedArgs);
                            }
                        }
                        Keyword::DigitalRead | Keyword::AnalogRead | Keyword::DigitalWrite => {
                            expected = Some(ExpectedArgument::Number);
                            command = match kw {
                                Keyword::DigitalRead => BasicCommand::DigitalRead(None),
                                Keyword::DigitalWrite => BasicCommand::DigitalWrite(None, None),
                                Keyword::AnalogRead => BasicCommand::AnalogRead(None),
                                _ => unreachable!(),
                            };
                        }
                        _ => return Err(InterpretationError::UnimplementedToken),
                    },
                    Token::String(str) => match command {
                        BasicCommand::Print(None) => {
                            if let Some(existing_str_idx) = existing_string_idx {
                                string_table[existing_str_idx as usize] = None;
                            }
                            let new_idx = put_string_table(str, string_table)
                                .map_err(|_| InterpretationError::StringTableFull)?;
                            command = BasicCommand::Print(Some(Expression::String(new_idx)));
                            expected = None;
                        }
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                    Token::Number(num) => match command {
                        BasicCommand::Goto(ref mut goto_dest)
                        | BasicCommand::If(
                            Expression::Boolean(Some(_), Some(_), Some(_)),
                            ref mut goto_dest,
                        ) if goto_dest.is_none() => {
                            *goto_dest = Some(num);
                            expected = None;
                        }
                        BasicCommand::Let(Some(_), ref mut expr) => match expr {
                            None => {
                                let mut tokens = ArrayVec::<MathToken, 5>::new();
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
                                let mut tokens = ArrayVec::<MathToken, 5>::new();
                                tokens.push(MathToken::Literal(num));
                                *expr = Some(Expression::Math(tokens));
                            }
                            Some(Expression::Math(ref mut nums)) => {
                                nums.try_push(MathToken::Literal(num))
                                    .map_err(|_| InterpretationError::StackFull)?;
                            }
                            Some(Expression::Boolean(Some(_), Some(_), ref mut expr))
                                if expr.is_none() =>
                            {
                                *expr = Some(MathToken::Literal(num))
                            }
                            _ => return Err(InterpretationError::UnexpectedArgs),
                        },
                        BasicCommand::If(
                            Expression::Boolean(ref mut expr, None, None)
                            | Expression::Boolean(Some(_), Some(_), ref mut expr),
                            None,
                        ) => *expr = Some(MathToken::Literal(num)),
                        BasicCommand::AnalogRead(ref mut pin)
                        | BasicCommand::DigitalRead(ref mut pin)
                        | BasicCommand::DigitalWrite(ref mut pin, None)
                            if pin.is_none() =>
                        {
                            *pin = Some(
                                num.try_into()
                                    .map_err(|_| InterpretationError::UnexpectedArgs)?,
                            );
                            match command {
                                BasicCommand::AnalogRead(_) | BasicCommand::DigitalRead(_) => {
                                    expected = None;
                                }
                                BasicCommand::DigitalWrite(_, _) => (),
                                _ => unreachable!(),
                            }
                        }
                        BasicCommand::DigitalWrite(Some(_), ref mut val) if val.is_none() => {
                            *val = Some(match num {
                                0 => false,
                                1 => true,
                                _ => return Err(InterpretationError::UnexpectedArgs),
                            });
                            expected = None;
                        }
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
                            let mut numbers = ArrayVec::<MathToken, 5>::new();
                            numbers.push(MathToken::Variable(var_idx));
                            *expr = Some(Expression::Math(numbers));
                        }
                        BasicCommand::Let(ref mut var, None) if var.is_none() => {
                            *var = Some(var_idx);
                            expected = Some(ExpectedArgument::Assignment);
                        }
                        BasicCommand::Let(Some(_), ref mut expr) if expr.is_none() => {
                            let mut numbers = ArrayVec::<MathToken, 5>::new();
                            numbers.push(MathToken::Variable(var_idx));
                            *expr = Some(Expression::Math(numbers));
                        }
                        BasicCommand::If(
                            Expression::Boolean(ref mut expr, None, None)
                            | Expression::Boolean(Some(_), Some(_), ref mut expr),
                            None,
                        ) => *expr = Some(MathToken::Variable(var_idx)),
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
                    Token::ComparisionOperator(op) => match command {
                        BasicCommand::Let(Some(_), None) if op == ComparisionOperator::Equal => {
                            expected = Some(ExpectedArgument::Expression);
                            continue;
                        }
                        BasicCommand::If(Expression::Boolean(_, ref mut operator, _), None) => {
                            *operator = Some(op);
                        }
                        BasicCommand::Print(Some(ref mut expr)) => {
                            if let Expression::Math(tokens) = expr {
                                if tokens.len() == 1 {
                                    *expr = Expression::Boolean(Some(tokens[0]), Some(op), None);
                                } else {
                                    return Err(InterpretationError::MathToBooleanFailed);
                                }
                            }
                        }
                        _ => return Err(InterpretationError::UnexpectedArgs),
                    },
                }
            } else {
                return Err(InterpretationError::UnexpectedArgs);
            }
        }

        match command {
            BasicCommand::Print(Some(ref mut expr)) => {
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
            BasicCommand::Let(Some(_), Some(ref mut expr)) => {
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
    fn preceedes(self, other: Self) -> bool {
        matches!(
            (self, other),
            (Self::Multiply | Self::Divide, Self::Plus | Self::Minus)
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

    fn is(&self, expected: ExpectedArgument) -> bool {
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
