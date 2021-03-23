use std::{io::{Write}};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn main() {
	let euler: Vec<(String, Box<dyn Fn() -> String>)> = vec![
		(//1
			String::from("Find the sum of all the multiples of 3 or 5 below 1000."),
			Box::new(|| {
				let mut sum = 0;
				for i in 1..1000 {
					if i % 3 == 0 || i % 5 == 0 {
						sum += i;
					}
				}

				sum.to_string()
			}),
		),
		(//2
			String::from("By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."),
			Box::new(|| {
				let mut fib = vec![1,2];

				while fib[fib.len() - 1] <= 4000000 {
					let l = fib.len();
					fib.push(fib[l-2] + fib[l-1]);
				}

				let mut sum = 0;

				for x in fib {
					if x % 2 == 0 {
						sum += x;
					}
				}

				sum.to_string()
			}),
		),
		(//3
			String::from("What is the largest prime factor of the number 600851475143?"),
			Box::new(|| {
				let num: u64 = 600851475143;
				let mut factors = vec![];

				let mut n = num;//numerator
				let mut d = 2;//denominator
				while d <= n {
					while n % d == 0 {
						n /= d;
						factors.push(d);
					}

					d += 1;
				}
				
				(*factors.last().unwrap() as i32).to_string()
			}),
		),
		(//4
			String::from("Find the largest palindrome made from the product of two 3-digit numbers."),
			Box::new(|| {
				let mut r = 0;

				let is_palindrome = |x: String|  {
					let mut is_good = true;

					let top = x.len() - 1;

					let chars: Vec<char> = x.chars().collect();

					let mut i = 0;
					while i*2 < x.len() && is_good{
						if chars[i] != chars[top - i] {
							is_good = false;
						}

						i += 1;
					}

					is_good
				};

				let mut i = 999;
				let mut found = false;
				while !found {
					for j in ((i-100)..=i).rev() {
						let p = i*j;
						if is_palindrome(p.to_string()) {
							r = p;
							found = true;
							break;
						}
					}
					i -= 1;
				}

				r.to_string()
			}),
		),
		(//5
			String::from("What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"),
			Box::new(|| {
				let mut r;
				let iter = 11..=20;//can start at 11 because 11..=20 includes 1..=10 as factors
				let range = *iter.clone().collect::<Vec<i32>>().last().unwrap();
				
				r = range * 10000000;//high starting value to save time on computation
				while !(iter.clone()).all(|x| r % x == 0) {
					r += range;
				}

				r.to_string()
			}),
		),
		(//6
			String::from("Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."),
			Box::new(|| {
				let vec_range = 1..=100;

				let sum_of_squares: i32 = vec_range.clone().fold(0, |acc, x| acc + (x*x));
				let square_of_sum = (|x|x*x)(vec_range.fold(0, |acc, x| acc + x));//(closure)(call)

				(square_of_sum - sum_of_squares).to_string()
			}),
		),
		(//7
			String::from("What is the 10,001st prime number?"),
			Box::new(|| {
				let mut primes = vec![2];
				let mut c = 3;

				while primes.len() < 10001 {
					let mut is_prime = true;

					for x in primes.clone() {
						if c % x == 0 {
							is_prime = false;
							break;
						}

						if x*x > c {
							break;
						}
					}

					if is_prime {
						primes.push(c);
					}

					c += 2;
				}

				(*primes.last().unwrap()).to_string()
			}),
		),
		(//8
			String::from("Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?"),
			Box::new(|| {
				let bignum = String::from("7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450");
				let bignum = bignum.chars().into_iter().map(|x| x.to_digit(10).unwrap() as i32).collect::<Vec<i32>>();

				let series_size = 13;

				let mut largest = (0,0,vec![]);
				for i in 0..(bignum.len() - series_size) {
					let mut p: u128 = 1;
					let mut series = vec![];
					for j in i..i+series_size {
						p *= bignum[j] as u128;
						series.push(bignum[j]);
					}

					if p > largest.1 {
						largest = (i as i32,p,series);
					}
				}
				
				(largest.1).to_string()
			}),
		),
		(//9
			String::from("There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."),
			Box::new(|| {
				let f = |(u,v)| -> [i32; 3] {//snagged this formula from https://www.youtube.com/watch?v=QJYmyhnaaek
					let a = (u*u) - (v*v);
					let b = 2 * u * v;
					let c = (u*u) + (v*v);

					[a,b,c]
				};

				let mut r: i32 = 0;

				'outer: for x in 1.. {
					for y in 1..x {
						let triplet = f((x,y));
						if triplet.iter().sum::<i32>() == 1000{
							r = triplet.iter().product();
							break 'outer;
						}
					}
				}

				r.to_string()
			}),
		),
        (//10
            String::from("Find the sum of all the primes below two million."),
            Box::new(|| {
                let mut primes = (2..2000000).collect::<Vec<u64>>();
				let mut sum = 0;

				//sieve
				for i in 0..primes.len() {
					let n = primes[i] as usize;
					if n != 0 {
						sum += n;

						for j in ((i+n)..primes.len()).step_by(n) {
							primes[j] = 0;
						}
					}
				}

				sum.to_string()
            }),
        ),
		(//template
			String::from("Coming Soon"),
			Box::new(|| {
				7.to_string()
			}),
		),
	];

	let args = std::env::args().collect::<Vec<String>>();

	let write_problem = |i: usize| {
		write_text(format!("{}> {}", i+1, euler[i].0));
		write_num(euler[i].1().to_string());
	};

	match args.len() {
		1 => {
			for i in 0..euler.len() {
				write_problem(i);
			}
		},
		2 => {
			match args[1].parse::<usize>() {
				Ok(n) => {
					if (0..euler.len()).any(|x| x == n-1) {
						write_problem(n-1);
					}else{
						println!("Error: invalid problem number\nPlease input a number between ");
					}
				},
				_ => panic!("Error: Argument must be a valid integer"),
			}
		},
		_ => panic!("ERROR: invalid number of arguments"),
	}
}

///prints colored text formatted for numbers
fn write_num(arg: String) {
	write_color(Color::Blue);
	println!("{}", arg);
	reset_color();
}

///prints colored text formatted for text
fn write_text(arg: String) {
	write_color(Color::Magenta);
	println!("{}", arg);
	reset_color();
}

///resets the color for terminal ouptut to white
fn reset_color() {
	write_color(Color::White);
}

///changes the color of the terminal
fn write_color(fg: Color) {
	let f = || {
		let mut stdout = StandardStream::stdout(ColorChoice::Always);
		let new_spec = ColorSpec::new()
													.set_fg(Some(fg))
													.clone();
		stdout.set_color(&new_spec)?;
		write!(&mut stdout, "")
	};

	match f() {
		_ => (),
	};
}

