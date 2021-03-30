use std::{fmt, ops::{Add, AddAssign, Mul, MulAssign}};

#[derive(Clone)]
pub struct BigNum {
	pub num: Vec<u8>,
}

#[allow(dead_code)]
struct BigNumIter {
    curr: BigNum,
    next: BigNum,
    limit: BigNum,
}

#[allow(dead_code)]
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

    pub fn zero() -> BigNum {
        BigNum {
            num: vec![0]
        }
    }

    pub fn one() -> BigNum {
        BigNum {
            num: vec![1]
        }
    }
}

impl Add for BigNum {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut result = self.num.clone();

        for i in 0..rhs.num.len() {
            if i < result.len() {
                result[i] += rhs.num[i];
            } else {
                result.push(rhs.num[i]);
            }
        }

        let mut carry = 0;
        for i in 0..result.len() {
            result[i] += carry;

            let rem = result[i] % 10;
            carry = result[i] / 10;

            result[i] = rem;
        }

        if carry > 0 {
            result.push(carry);
        }

        BigNum {
            num: result
        }
    }
}

impl AddAssign for BigNum {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.clone() + rhs;
    }
}

impl Mul for BigNum {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if rhs == BigNum::one() {
            return self;
        }

        if rhs == BigNum::zero() || self == BigNum::zero() {
            return BigNum::zero();
        }

        let mut partials = vec![];
        for x in self.clone().num {
            let mapped = rhs.num.iter().map(|a| a * x).collect::<Vec<u8>>();
            let mut partial = vec![];
            let mut carry = 0;
            for y in mapped {
                let a = y + carry;

                let rem = a % 10;
                carry = a / 10;
                partial.push(rem);
            }
            if carry > 0 {
                partial.push(carry);
            }
            partials.push(partial);
        }

        //println!("p> {}*{} = {:?}", self, rhs, partials);

        let mut returnme = BigNum::zero();

        for (i,p) in partials.iter().enumerate() {
            let mut big_p = vec![0;i];
            big_p.extend(p);
            returnme += BigNum{num:big_p};
        }

        returnme
    }
}

impl MulAssign for BigNum {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.clone() * rhs
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
        let cases = vec![
            vec![3,4],
            vec![132,345],
            vec![13,456,7,3],
        ];

        for c in cases {
            let control = c.iter().fold(1,|acc,x| acc * x);
            let result = c.iter()
                                    .map(|x| BigNum::from(x.to_string()))
                                    .collect::<Vec<BigNum>>()
                                    .iter()
                                    .fold(BigNum::one(), |acc, x| acc * x.clone());
            assert_eq!(control.to_string(), result.to_string());
        }
    }

    // todo: add and test iterator functions
}