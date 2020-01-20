module JS

open FStar.Tactics
open Data.Serialize
open Data.Serialize.Rep
module M = FStar.OrdMap

module S = FStar.String
module L = FStar.List.Tot







let primsOpsJS = 
"
let JS_jsEval = s => eval(s);

function match(branches, value){
    let o = {};
    let helper = (pat, value) => {
	if(pat instanceof Array){      // Pat_Cons
	    let [cons, ...args] = pat;

            if(!(value instanceof Array))
              return false;

            if(cons == 'Prims_Cons'){
              return value.length
                  && helper(args[0], value[0])
                  && helper(args[1], value.slice(1));
            }else if (cons == 'Prims_Nil'){
              return !value.length;
            }

	    return (value[0] == cons
		    && args.every(
			(pat, i) =>
			    helper(pat, value[i])
		    )
		   );
	}else if ((pat || {}).bv){// Pat_Var|Wild|Dot_Term 
	    o[pat.bv] = value;
	    return true;
	}else{                         // Pat_Constant
	    return value === pat;
	}
	return false;
    };
    for(let [pat, term] of branches){
	if(helper(pat, value))
	    return term(o);
    }
    throw 'match unsuccessful';
}
let _MkMK_ = (name, arity) =>
    {
        if(name == 'Prims_Nil')
          return [];
        if(name == 'Prims_Cons')
          return head => tail => [head, ...tail];
	let o = l => [name, ...l];
	for(let i=0; i < arity; i++){
	    let prev = o;
	    o = l => x => prev([x, ...l]);
	}
	return o([]);
    }
let _Y = (def, body) => {
	let Y = () => def(() => Y ());
	return body(Y());
    }
let Prims_op_Subtraction = x => y => x - y;
let Prims_op_Addition = x => y => x + y;
let Prims_op_GreaterThan = x => y => x > y; 
let Prims_op_Equality = x => y => x == y; 
let FList_toArray = l => {
  let a = [];
  while(l[0] == 'Prims_Cons'){
    a.push(l[2]);
    l = l[1];
  }
  return a.reverse();
} 

"

let writeToFile file content
  = let _ = launch_process "sh" ["-c"; "cat - > " ^ file] content in
    ()   

irreducible let jsEval (s: string): unit = ()

let rec fib (n: nat) =
  if n > 2
  then fib (n - 1) + fib (n - 2)  
  else 1

let rec fibs (n: nat)
  = if n = 0
    then []
    else fib n::fibs (n-1)

let jsOfFib: string = _ by (
  let s = jsOfFv (fvOf (`fib)) in
  let s' = jsOfFv (fvOf (`fibs)) in
  writeToFile "test.js" (primsOpsJS ^ s ^ s' ^ "

console.log(JS_fibs(34));
//console.log(JSON.stringify(FList_toArray(JS_fibs(6)), null, 4));
  ");
  exact (quote s)
)



