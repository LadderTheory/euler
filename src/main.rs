use std::{collections::HashMap, io::{Write}, u128, usize};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn main() {
	let mut euler: HashMap<usize, (String, Box<dyn Fn() -> String>)> = HashMap::new();
	euler.insert(
		1,
		(
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
		)
	);
	euler.insert(
		2,
		(
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
		)
	);
	euler.insert(
		3,
		(
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
		)
	);
	euler.insert(
		4,
		(
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
	);
	euler.insert(
		5,
		(
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
		)
	);
	euler.insert(
		6,
		(//6
			String::from("Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."),
			Box::new(|| {
				let vec_range = 1..=100;

				let sum_of_squares: i32 = vec_range.clone().fold(0, |acc, x| acc + (x*x));
				let square_of_sum = (|x|x*x)(vec_range.fold(0, |acc, x| acc + x));//(closure)(call)

				(square_of_sum - sum_of_squares).to_string()
			}),
		)
	);
	euler.insert(
		7,
		(
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
			})
		)
	);
	euler.insert(
		8,
		(
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
			})
		)
	);
	euler.insert(
		9,
		(
			String::from("There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."),
			Box::new(|| {
				let find_k = |h,s| {//snagged this formula from https://www.youtube.com/watch?v=QJYmyhnaaek
					// a = h^2 - k^2;
					// b = 2hk;
					// c = h^2 + k^2;
					// a + b + c = 2(h^2 hk) = s
					// formula to find k from h & s:
					// -h + (s/2h) = k
					(0-h) + (s/(2*h))
				};

				let find_sum = |h,k| {
					2 * ((h*h) + (h*k))
				};

				let find_product = |h: i32,k: i32| {
					// abc = (h^2 - k^2)(2hk)(h^2 + k^2) = 2h^5k - 2hk^5
					(2 * h.pow(5) * k) - (2 * h * k.pow(5))
				};

				let mut hk: Vec<i32> = Vec::new();

				let sum = 1000;

				for h in 2.. {
					let k = find_k(h, sum);
					if k >= 1 {
						if find_sum(h,k) == sum {
							if k % 1 == 0 {
								hk = vec![h,k];
							}
						}
					}else{
						break;
					}
					
				}

				find_product(hk[0],hk[1]).to_string()
			})
		)
	);
	euler.insert(
		10,
        (
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
            })
		)
	);
	euler.insert(
		12,
		(
			String::from("What is the value of the first triangle number to have over five hundred divisors?"),
			Box::new(|| {
				let primelist = seive(500);

				let prime_factors = |x| {
					let mut divisors = 1;

					let mut remains = x;
					for i in primelist {
						let mut exponent = 1;
						while remains % i == 0 {
							exponent += 1;
							remains /= i;
						}
						divisors *= exponent;

						if remains == 1 {
							break;
						}
					}

					divisors
				};

				let mut t = 1;
				let mut nn = 2;
				for _ in 0.. {
					let pf = prime_factors.clone()(t);
					if pf> 500 {
						break;
					}
					t += nn;
					nn += 1;
				}

				format!("{:?}", t)
			})
		)
	);
	euler.insert(
		13,
		(
			String::from("Work out the first ten digits of the sum of the following one-hundred 50-digit numbers."),
			Box::new(|| {
				let big_ol_boy = String::from("37107287533902102798797998220837590246510135740250463769376774900097126481248969700780504170182605387432498619952474105947423330951305812372661730962991942213363574161572522430563301811072406154908250230675882075393461711719803104210475137780632466768926167069662363382013637841838368417873436172675728112879812849979408065481931592621691275889832738442742289174325203219235894228767964876702721893184745144573600130643909116721685684458871160315327670386486105843025439939619828917593665686757934951621764571418565606295021572231965867550793241933316490635246274190492910143244581382266334794475817892575867718337217661963751590579239728245598838407582035653253593990084026335689488301894586282278288018119938482628201427819413994056758715117009439035398664372827112653829987240784473053190104293586865155060062958648615320752733719591914205172558297169388870771546649911559348760353292171497005693854370070576826684624621495650076471787294438377604532826541087568284431911906346940378552177792951453612327252500029607107508256381565671088525835072145876576172410976447339110607218265236877223636045174237069058518606604482076212098132878607339694128114266041808683061932846081119106155694051268969251934325451728388641918047049293215058642563049483624672216484350762017279180399446930047329563406911573244438690812579451408905770622942919710792820955037687525678773091862540744969844508330393682126183363848253301546861961243487676812975343759465158038628759287849020152168555482871720121925776695478182833757993103614740356856449095527097864797581167263201004368978425535399209318374414978068609844840309812907779179908821879532736447567559084803087086987551392711854517078544161852424320693150332599594068957565367821070749269665376763262354472106979395067965269474259770973916669376304263398708541052684708299085211399427365734116182760315001271653786073615010808570091499395125570281987460043753582903531743471732693212357815498262974255273730794953759765105305946966067683156574377167401875275889028025717332296191766687138199318110487701902712526768027607800301367868099252546340106163286652636270218540497705585629946580636237993140746255962240744869082311749777923654662572469233228109171419143028819710328859780666976089293863828502533340334413065578016127815921815005561868836468420090470230530811728164304876237919698424872550366387845831148769693215490281042402013833512446218144177347063783299490636259666498587618221225225512486764533677201869716985443124195724099139590089523100588229554825530026352078153229679624948164195386821877476085327132285723110424803456124867697064507995236377742425354112916842768655389262050249103265729672370191327572567528565324825826546309220705859652229798860272258331913126375147341994889534765745501184957014548792889848568277260777137214037988797153829820378303147352772158034814451349137322665138134829543829199918180278916522431027392251122869539409579530664052326325380441000596549391598795936352974615218550237130764225512118369380358038858490341698116222072977186158236678424689157993532961922624679571944012690438771072750481023908955235974572318970677254791506150550495392297953090112996751986188088225875314529584099251203829009407770775672113067397083047244838165338735023408456470580773088295917476714036319800818712901187549131054712658197623331044818386269515456334926366572897563400500428462801835170705278318394258821455212272512503275512160354698120058176216521282765275169129689778932238195734329339946437501907836945765883352399886755061649651847751807381688378610915273579297013376217784275219262340194239963916804498399317331273132924185707147349566916674687634660915035914677504995186714302352196288948901024233251169136196266227326746080059154747183079839286853520694694454072476841822524674417161514036427982273348055556214818971426179103425986472045168939894221798260880768528778364618279934631376775430780936333301898264209010848802521674670883215120185883543223812876952786713296124747824645386369930090493103636197638780396218407357239979422340623539380833965132740801111666627891981488087797941876876144230030984490851411606618262936828367647447792391803351109890697907148578694408955299065364044742557608365997664579509666024396409905389607120198219976047599490197230297649139826800329731560371200413779037855660850892521673093931987275027546890690370753941304265231501194809377245048795150954100921645863754710598436791786391670211874924319957006419179697775990283006991536871371193661495281130587638027841075444973307840789923115535562561142322423255033685442488917353448899115014406480203690680639606723221932041495354150312888033953605329934036800697771065056663195481234880673210146739058568557934581403627822703280826165707739483275922328459417065250945123252306082291880205877731971983945018088807242966198081119777158542502016545090413245809786882778948721859617721078384350691861554356628840622574736922845095162084960398013400172393067166682355524525280460972253503534226472524250874054075591789781264330331690");
				let mut boys = vec![];

				let mut s = String::new();
				for c in big_ol_boy.chars() {
					s = match c.to_digit(10) {
						Some(x) => format!("{}{}", s, x),
						None => s,
					};

					if s.len() == 50 {
						boys.push(s);
						s = String::new();
					}
				}

				let limit= (10 as u64).pow(10);
				let mut sum = 0;
				
				for s in &boys {
					let first10 = String::from(&s[(s.len() - 10)..]);
					sum += match first10.parse::<u64>() {
						Ok(x) => x,
						_ => 0,
					};
					//println!("{:?}", (s, first10));
				}

				while sum > limit {
					sum -= limit;
				}

				format!("{:?}", sum)
			})
		)
	);

	let args = std::env::args().collect::<Vec<String>>();

	let write_problem = |i: usize| {
		match euler.get(&i) 
		{
			Some(x) => {
				write_text(format!("{}> {}", i, x.0));
				write_num(x.1().to_string());
			},
			None => write_text(format!("{}> PROBLEM NOT SOLVED", i)),
			//write_text(format!("{}> {}", i+1, euler[i].0));
			//write_num(euler[i].1().to_string());
		}
		
	};

	println!("sigma {}", sigma(
		5,
		Box::new(|x| 8 * x),
		1
	));

	let mut keys= euler.keys().cloned().collect::<Vec<usize>>();
	keys.sort();

	match args.len() {
		1 => {
			for i in keys {
				write_problem(i);
			}
		},
		2 => {
			match args[1].parse::<usize>() {
				Ok(n) => {
					write_problem(n);
				}
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

fn sigma(n: i32, a: Box<dyn Fn(i32) -> i32>, k: i32) -> i32 {
	(k..=n).fold(0, |acc, x| acc + a(x))
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

fn seive(l: u32) -> Vec<u32>{
	let mut s= (2..=l).collect::<Vec<u32>>();
	let mut primes = vec![];
	for i in 0..s.len() {
		if s[i] != 0 {
			primes.push(s[i]);
			let iter = ((i + (s[i] as usize))..s.len()).step_by(s[i] as usize);
			for j in iter {
				s[j] = 0;
			}
		}
	}

	primes
}