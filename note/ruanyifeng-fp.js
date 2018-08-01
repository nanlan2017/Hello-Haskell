/*-------------------------- 范畴 -----------------------------------------*/
/*
...上图中，各个点与它们之间的箭头，就构成一个范畴。
箭头表示范畴成员之间的关系，正式的名称叫做"态射"（morphism）。
范畴论认为，同一个范畴的所有成员，就是不同状态的"变形"（transformation）。通过"态射"，一个成员可以变形成另一个成员。

"范畴"是满足某种变形关系的所有对象。
*/


/*
Category是一个类，也是一个容器，里面包含一个值（this.val）和一种变形关系（addOne）。
你可能已经看出来了，这里的范畴，就是所有彼此之间相差1的数字。
*/
class CategoryItem {
    constructor(val) {
        this.val = val;
    }

    addOne(x) {
        return x + 1;
    }
}

/*---------------------------- 合成和柯里化 -------------------------------------*/
// 范畴论使用函数，表达范畴之间的关系。
const compose = function (f, g) {
    return function (x) {
        return f(g(x));
    };
}
//--------------------------------
// 柯里化之前
function add(x, y) {
    return x + y;
}

add(1, 2) // 3
// 柯里化之后
function addX(y) {
    return function (x) {
        return x + y;
    };
}

addX(2)(1) // 3


/**===============>  运算就统一成了 一系列单参数函数的组合*/
/*---------------------------- 函子 -------------------------------------*/
/*
Functor是一个函子，它的map方法接受函数f作为参数，然后返回一个新的函子，里面包含的值是被f处理过的（f(this.val)）

▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ 函子是一个 容器，就是可以把一个 Context 外的函数“拎进来”（通过其map 接口） 作用的容器
*/
class Functor {
    constructor(val) {
        this.val = val;
    }

    /*比如var 代表一个人，这个f 完成可能是“取该人喜欢的食物”（人->食物），则生成了了食物的容器 （▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇而不是修改原容器！！！） */
    map(f) {
        return new Functor(f(this.val));
    }
}
/*
一般约定，函子的标志就是容器具有map方法。该方法将容器里面的每一个值，映射到另一个容器。
*/
(new Functor(2)).map(function (x) {
    return x + 2;
});
// Functor(4)
(new Functor('flamethrowers')).map(function (s) {
    return s.toUpperCase();
});
// Functor('FLAMETHROWERS')
(new Functor('bombs')).map(_.concat(' away')).map(_.prop('length'));
// Functor(10)

/*
上面的例子说明，函数式编程里面的运算，都是通过函子完成，即运算不直接针对值，而是针对这个值的容器----函子。
  
▇▇▇▇▇▇▇▇▇▇▇▇函子本身具有对外接口（map方法），各种函数就是运算符，通过接口接入容器，引发容器里面的值的变形。
因此，学习函数式编程，实际上就是学习函子的各种运算。

//▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇ wjh+ : 有了函子，就可以为一个值附加 上下文。并且不影响对其中的值进行运算。（还可以增加更多的运算辅助）
由于可以把运算方法封装在函子里面，所以又衍生出各种不同类型的函子，有多少种运算，就有多少种函子。
函数式编程就变成了运用不同的函子，解决实际问题。

------------------------------------
? 3+3 
*/
Functor.of = function (val) {
    return new Functor(val);
};

Functor.of(2).map(function (x) {
    return x + 2;
});
// Functor(4)

//-------------------------------------------------------------
--报错
Functor.of(null).map(function (s) {
    return s.toUpperCase();
});

// TypeError
class Maybe extends Functor {
    map(f) {
        return this.val != null
            ? Maybe.of(f(this.val))
            : Maybe.of(null);
    }
}

//---------------------------------------------------------
//Either 函子内部有两个值：左值（Left）和右值（Right）。右值是正常情况下使用的值，左值是右值不存在时使用的默认值。
class Either extends Functor {
    constructor(left, right) {
        this.left = left;
        this.right = right;
    }

    map(f) {
        return this.right ?
            Either.of(this.left, f(this.right)) :
            Either.of(f(this.left), this.right);
    }
}

Either.of = function (left, right) {
    return new Either(left, right);
};
//------------TEST
var addOne = function (x) {
    return x + 1;
};

Either.of(5, 6).map(addOne);
// Either(5, 7);
Either.of(1, null).map(addOne);
// Either(2, null);



class Functor {
    val
    constructor(val) {
        this.val = val;
    }

    map(f) {
        return new Functor(f(this.val));
    }
}

/*
▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇函子里面包含的值，可能是函数
（比如 list 里包含的是函数）
*/
function addTwo(x) {
    return x + 2;
}

const A = Functor.of(2);
const B = Functor.of(addTwo)  //

/**
上面代码中，函子A内部的值是2，函子B内部的值是函数addTwo。

有时，▇▇▇▇我们想让函子B内部的函数，可以使用函子A内部的值进行运算。这时就需要用到 ap 函子。

___> 即，map 接口进来的可能是一个“盒子”中的函数。 得先将其中的f 从里面取出来。 
*/
class Ap extends Functor {
    val_function

    /*
    map(f) { return new Functor(f(this.val));}
    */

    ap(F) {
        return Ap.of(this.val(F.val));   /* Ap这种盒子新增了一个ap()接口， 这个接口可以 收一个 Functor(盒子）, 取出Functor 盒子里的值，放到新的 Ap 盒子里*/
    }
    /* ▇▇▇▇▇▇▇▇▇▇注意： 上面的 this.val 是一个函数。！！！▇▇▇▇▇▇▇▇▇▇*/
}
Ap.of(addTwo).ap(Functor.of(2))
// Ap(4)

function add(x) {
    return function (y) {
        return x + y;
    };
}

Ap.of(add).ap(Maybe.of(2)).ap(Maybe.of(3));
// Ap(5)
Ap.of(add(2)).ap(Maybe.of(3));
/*================================================================================================ */


class Monad extends Functor {
    val


    map(f) {
        return new Monad(f(val));   /* f 作用于 val,  返回的结果在盒子(A)))中。  然后又被 new Monad再包一层，成为嵌套盒子-----> 最后能返回盒子 A */
    }

    //-------------------------------------
    /* Monad 盒子的join()接口，会返回盒子中的值（褪去盒子这层）*/
    join() {
        return val;
    }

    /* 总是返回一个单层的Functor盒子 :
    
    上面代码中，
    ▇▇▇▇▇如果函数f返回的是一个函子，
    ——> 那么this.map(f)就会生成一个嵌套的函子。
    ——————> 所以，join方法保证了flatMap方法总是返回一个单层的函子。
    这意味着嵌套的函子会被铺平（flatten）。
    */
    flatMap(f) {  // f 是一个会返回 盒子的函数 ..  
        return map(f).join(); /*return   new Monad( f(val) ).join() */
    }
}
/*============================================================*/
var fs = require('fs');

var readFile = function (filename) {
    return new IO(function () {
        return fs.readFileSync(filename, 'utf-8');
    });
};

var print = function (x) {
    return new IO(function () {
        console.log(x);
        return x;
    });
}
