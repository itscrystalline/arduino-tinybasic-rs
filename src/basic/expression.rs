use arrayvec::ArrayVec;
use bincode::{Decode, Encode};

use super::{
    interpreter::MathToken,
    lexer::{ComparisionOperator, MathOperator},
};

#[derive(Clone, Encode, Decode)]
pub enum Expression {
    String(u8),
    Math(#[bincode(with_serde)] ArrayVec<MathToken, 5>),
    Boolean(
        #[bincode(with_serde)] Option<MathToken>,
        Option<ComparisionOperator>,
        #[bincode(with_serde)] Option<MathToken>,
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
    pub fn evaluate_math<const N: usize>(
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

    pub fn evaluate_boolean(
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
