let fib = n => 
    n > 2 ? fib(n-1) + fib(n-2) : 1;

let fibs = n =>
    n > 0 ? [fib(n), ...fibs(n-1)] : [];

console.log(fibs(34));

