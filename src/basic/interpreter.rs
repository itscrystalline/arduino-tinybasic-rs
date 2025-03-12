use core::char;

use arduino_hal::prelude::*;
use arrayvec::ArrayVec;
use ufmt::{uDebug, uDisplay, uwrite, uwriteln};

use crate::{
    arduino::{PinError, Pins},
    basic::lexer::{ComparisionOperator, ExpectedArgument, Keyword, Token},
    put_string_table, Serial, E_DIV_ZERO, E_INCOMPLETE_EXPR, E_NOT_BOOL, E_NUM_STACK_FULL,
    E_PIN_NOT_INPUT, E_PIN_NOT_OUTPUT, E_PIN_NOT_PWM, E_PIN_RESERVED, E_PIN_UNUSABLE,
    E_UNIMPLEMENTED, E_VAL_OVERFLOW, E_VAL_UNDERFLOW, PROGRAM_LENGTH,
};

use super::{
    expression::{EvaluationError, Expression},
    lexer::{MathOperator, String, TokenBuffer},
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
pub enum BasicCommand {
    AnalogRead(Option<u8>, Option<MathToken>),
    DigitalRead(Option<u8>, Option<MathToken>),
    DigitalWrite(Option<u8>, Option<MathToken>),
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
            BasicCommand::Print(Some(expr)) => match expr {
                Expression::String(table_idx) => {
                    string_table[*table_idx as usize]
                        .unwrap()
                        .chars()
                        .for_each(|ch| uwrite!(serial, "{}", ch).unwrap_infallible());
                    uwriteln!(serial, "\r").unwrap_infallible();
                }
                Expression::Math(numbers) => {
                    let result = Expression::evaluate_math(numbers.clone(), variables);
                    match result {
                        Ok(res) => uwriteln!(serial, "{}\r", res),
                        Err(e) => match e {
                            EvaluationError::Incomplete => {
                                uwriteln!(serial, "{}", E_INCOMPLETE_EXPR)
                            }
                            EvaluationError::Overflow => uwrite!(serial, "{}", E_VAL_OVERFLOW),
                            EvaluationError::Underflow => {
                                uwriteln!(serial, "{}", E_VAL_UNDERFLOW)
                            }
                            EvaluationError::DivideByZero => {
                                uwriteln!(serial, "{}", E_DIV_ZERO)
                            }
                            EvaluationError::StackFull => {
                                uwriteln!(serial, "{}", E_NUM_STACK_FULL)
                            }
                        },
                    }
                    .unwrap_infallible();
                }
                Expression::Boolean(Some(left), Some(cmp), Some(right)) => {
                    let res = Expression::evaluate_boolean(cmp, *left, *right, variables);
                    match res {
                        Ok(val) => uwriteln!(serial, "{}\r", val).unwrap_infallible(),
                        Err(e) => match e {
                            EvaluationError::Incomplete => {
                                uwriteln!(serial, "{}", E_INCOMPLETE_EXPR)
                            }
                            EvaluationError::Overflow => uwriteln!(serial, "{}", E_VAL_OVERFLOW),
                            EvaluationError::Underflow => {
                                uwriteln!(serial, "{}", E_VAL_UNDERFLOW)
                            }
                            EvaluationError::DivideByZero => {
                                uwriteln!(serial, "{}", E_DIV_ZERO)
                            }
                            EvaluationError::StackFull => {
                                uwriteln!(serial, "{}", E_NUM_STACK_FULL)
                            }
                        }
                        .unwrap_infallible(),
                    }
                }
                _ => uwriteln!(serial, "{}", E_INCOMPLETE_EXPR).unwrap_infallible(),
            },
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
                                uwriteln!(serial, "{}", E_INCOMPLETE_EXPR).unwrap_infallible()
                            }
                            EvaluationError::Overflow => {
                                uwriteln!(serial, "{}", E_VAL_OVERFLOW).unwrap_infallible()
                            }
                            EvaluationError::Underflow => {
                                uwriteln!(serial, "{}", E_VAL_UNDERFLOW).unwrap_infallible()
                            }
                            EvaluationError::DivideByZero => {
                                uwriteln!(serial, "{}", E_DIV_ZERO).unwrap_infallible()
                            }
                            EvaluationError::StackFull => {
                                uwriteln!(serial, "{}", E_NUM_STACK_FULL).unwrap_infallible()
                            }
                        };
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
                    Err(e) => {
                        match e {
                            EvaluationError::Incomplete => {
                                uwriteln!(serial, "{}", E_INCOMPLETE_EXPR)
                            }
                            EvaluationError::Overflow => uwriteln!(serial, "{}", E_VAL_OVERFLOW),
                            EvaluationError::Underflow => uwriteln!(serial, "{}", E_VAL_UNDERFLOW),
                            EvaluationError::DivideByZero => {
                                uwriteln!(serial, "{}", E_DIV_ZERO)
                            }
                            EvaluationError::StackFull => {
                                uwriteln!(serial, "{}", E_NUM_STACK_FULL)
                            }
                        }
                        .unwrap_infallible();
                        return BasicControlFlow::End;
                    }
                }
            }
            BasicCommand::AnalogRead(Some(pin), Some(MathToken::Variable(var_idx))) => {
                match pins.read_analog(*pin as usize) {
                    Ok(read) => {
                        variables[*var_idx as usize] = read;
                    }
                    Err(e) => match e {
                        PinError::NotInput => uwriteln!(serial, "{}", E_PIN_NOT_INPUT),
                        PinError::NotOutput => uwriteln!(serial, "{}", E_PIN_NOT_OUTPUT),
                        PinError::NotPwm => uwriteln!(serial, "{}", E_PIN_NOT_PWM),
                        PinError::NonAddressable => uwriteln!(serial, "{}", E_PIN_UNUSABLE),
                        PinError::Reserved => uwriteln!(serial, "{}", E_PIN_RESERVED),
                    }
                    .unwrap_infallible(),
                }
            }
            BasicCommand::DigitalRead(Some(pin), Some(MathToken::Variable(var_idx))) => {
                match pins.read_digital(*pin as usize) {
                    Ok(read) => {
                        variables[*var_idx as usize] = read as usize;
                    }
                    Err(e) => match e {
                        PinError::NotInput => uwriteln!(serial, "{}", E_PIN_NOT_INPUT),
                        PinError::NotOutput => uwriteln!(serial, "{}", E_PIN_NOT_OUTPUT),
                        PinError::NotPwm => uwriteln!(serial, "{}", E_PIN_NOT_PWM),
                        PinError::NonAddressable => uwriteln!(serial, "{}", E_PIN_UNUSABLE),
                        PinError::Reserved => uwriteln!(serial, "{}", E_PIN_RESERVED),
                    }
                    .unwrap_infallible(),
                }
            }
            BasicCommand::DigitalWrite(Some(pin), Some(value)) => {
                let bool_val = {
                    let val = match *value {
                        MathToken::Variable(idx) => variables[idx as usize],
                        MathToken::Literal(num) => num,
                        _ => {
                            uwriteln!(serial, "{}", E_INCOMPLETE_EXPR).unwrap_infallible();
                            return BasicControlFlow::End;
                        }
                    };
                    match val {
                        0 => false,
                        1 => true,
                        _ => {
                            uwriteln!(serial, "{}", E_NOT_BOOL).unwrap_infallible();
                            return BasicControlFlow::End;
                        }
                    }
                };
                if let Err(e) = pins.write_digital(*pin as usize, bool_val) {
                    match e {
                        PinError::NotInput => uwriteln!(serial, "{}", E_PIN_NOT_INPUT),
                        PinError::NotOutput => uwriteln!(serial, "{}", E_PIN_NOT_OUTPUT),
                        PinError::NotPwm => uwriteln!(serial, "{}", E_PIN_NOT_PWM),
                        PinError::NonAddressable => uwriteln!(serial, "{}", E_PIN_UNUSABLE),
                        PinError::Reserved => uwriteln!(serial, "{}", E_PIN_RESERVED),
                    }
                    .unwrap_infallible();
                }
            }
            BasicCommand::Rem => (),
            _ => uwriteln!(serial, "{}", E_UNIMPLEMENTED).unwrap_infallible(),
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
                        Keyword::Rem => {
                            single_command!(command, expected, BasicCommand::Rem);
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
                                Keyword::DigitalRead => BasicCommand::DigitalRead(None, None),
                                Keyword::DigitalWrite => BasicCommand::DigitalWrite(None, None),
                                Keyword::AnalogRead => BasicCommand::AnalogRead(None, None),
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
                        BasicCommand::AnalogRead(ref mut pin, None)
                        | BasicCommand::DigitalRead(ref mut pin, None)
                        | BasicCommand::DigitalWrite(ref mut pin, None)
                            if pin.is_none() =>
                        {
                            *pin = Some(
                                num.try_into()
                                    .map_err(|_| InterpretationError::UnexpectedArgs)?,
                            );
                            expected = Some(ExpectedArgument::Expression);
                        }
                        BasicCommand::DigitalWrite(Some(_), ref mut val) if val.is_none() => {
                            *val = Some(MathToken::Literal(num));
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
                        BasicCommand::DigitalWrite(Some(_), ref mut val)
                        | BasicCommand::DigitalRead(Some(_), ref mut val)
                        | BasicCommand::AnalogRead(Some(_), ref mut val)
                            if val.is_none() =>
                        {
                            *val = Some(MathToken::Variable(var_idx));
                            expected = None;
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
