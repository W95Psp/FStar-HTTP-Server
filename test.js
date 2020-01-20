
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


let JS_fib = ((n) => match([[true, ({}) => 

Prims_op_Addition(
JS_fib(

Prims_op_Subtraction(n)(1)))(
JS_fib(

Prims_op_Subtraction(n)(2)))], [{bv: "uu___"}, ({uu___}) => 1]], 

Prims_op_GreaterThan(n)(2)));

let JS_fibs = ((n) => match([[true, ({}) => 
_MkMK_("Prims_Nil",0)], [{bv: "uu___"}, ({uu___}) => 


_MkMK_("Prims_Cons",2)(
JS_fib(n))(
JS_fibs(

Prims_op_Subtraction(n)(1)))]], 


Prims_op_Equality(n)(0)));


console.log(JS_fibs(34));
//console.log(JSON.stringify(FList_toArray(JS_fibs(6)), null, 4));
  