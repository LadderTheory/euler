use std::{cmp, fmt, ops::{Add, Mul}, ptr::eq, string};

#[derive(Clone)]
pub struct BigNum {
	num: Vec<u8>,
}

struct BigNumIter {
    curr: BigNum,
    next: BigNum,
    limit: BigNum,
}

impl BigNum {
    pub fn from(arg: String) -> BigNum {
        let mut big: Vec<u8> = Vec::new();

        for c in arg.chars().rev() {
            big.push((c.to_digit(10).unwrap()) as u8);
        }

        BigNum {
            num: big
        }
    }

    pub fn zero() -> BigNum{
        BigNum {
            num: vec![0]
        }
    }

    pub fn one() -> BigNum{
        BigNum {
            num: vec![1]
        }
    }
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut result = vec![];

        for x in &self.num {
            result.push(*x);
        }

        println!("debug 1: {} {:?}", &self, result);

        for i in 0..rhs.num.len() {
            if i < result.len() {
                result[i] += rhs.num[i];
            } else {
                result.push(rhs.num[i]);
            }
        }

        println!("debug 2: {:?}", (rhs, result.clone()));

        let mut carry = 0;
        for i in 0..result.len() {
            result[i] += carry;

            let rem = result[i] % 10;
            carry = result[i] / 10;

            result[i] = rem;

            println!("debug 3: {:?}", (rem, carry, &result));
        }

        if carry > 0 {
            result.push(carry);
        }

        println!("{:?}", result);

        BigNum {
            num: result
        }
    }
}

impl PartialEq for BigNum {
    fn eq(&self, other: &Self) -> bool {
        self.num == other.num
    }
}

impl fmt::Debug for BigNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.num.iter().fold(String::from(""), |acc, x| format!("{}{}", x, acc)))
    }
}

impl fmt::Display for BigNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.num.iter().fold(String::from(""), |acc, x| format!("{}{}", x, acc)))
    }
}

#[cfg(test)]
mod tests {
    use super::BigNum;
    #[test]
    fn from_num() {
        assert_eq!("12345", BigNum::from("12345".to_string()).to_string());
    }

    #[test]
    fn addition() {
        let iter = 1..100;
        let nums = iter.clone().map(|x| BigNum::from(x.to_string()));
        assert_eq!(
            iter.sum::<u32>().to_string(), 
            nums.fold(BigNum::zero(), |acc, x| acc + x).to_string()
        );
    }

    #[test]
    fn zero() {
        assert_eq!("0", BigNum::zero().to_string())
    }

    #[test]
    fn one() {
        assert_eq!("1", BigNum::one().to_string())
    }

    #[test]
    fn multiplication() {
        let iter = 1..100;
        let nums = iter.clone().map(|x| BigNum::from(x.to_string()));
        assert_eq!(
            iter.sum::<u32>().to_string(), 
            nums.fold(BigNum::zero(), |acc, x| acc * x).to_string()
        );
    }

    //todo: add and test iterator functions
}