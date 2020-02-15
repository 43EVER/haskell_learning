// 1. 最开始的想法
let fact0 = f => n => n === 0 ? 1 : n * f(f)(n - 1)
console.log(fact0(fact0)(5))

// 2. 屌丝版太丑了，先使最外面那玩意简化一下
let w = f => f(f)
console.log(w(fact0)(5))

// 3. 继续化简，fact0里还有f(f) 这种东西，太丑
fact0 = f =>
            (g => n => n === 0 ? 1 : n * g(n - 1))(f(f))
// 但这样会爆栈，套个壳
fact0 = f =>
            (g => n => n === 0 ? 1 : n * g(n - 1))(v => f(f)(v))

// 5. 这个时候，中间一坨就是想要的递归函数的样子了，抽出来
fact0 = (f0 => f => f0(v => f(f)(v)))(g => n => n === 0 ? 1 : n * g(n - 1))

// 6. 套上w
let fact = w(
    (f0 => f => f0(v => f(f)(v)))
    (g => n => n === 0 ? 1 : n * g(n - 1))
)

// 7. 化简，把 递归函数提出来，变成 h
fact = (h =>
            w( (f0 => f => f0(v => f(f)(v)))(h)))
        (g => n => n === 0 ? 1 : n * g(n - 1))
console.log(fact(5))

// 8. 把 Y 提出来
let Y = (h => w( (f0 => f => f0(v => f(f)(v)))(h) ))

// 9. 把最后的h apply 一下
Y = (h => w(f => h(v => f(f)(v))))

// 10 把 w 展开
Y = (h => 
        (f => h(v => f(f)(v)))
        (f => h(v => f(f)(v)))
    )
// Y = lambda f . lambda x . (f (x x)) (f (x x))
// JS 里因为是立即求值，即先求参数，再进行 beta 变换，所以得套一层 v => f(f)(v)

console.log(Y(g => n => n === 0 ? 1 : n * g(n - 1))(5))

// 所以 Y 的目的是什么？
// 解决了一个问题，更优雅的引用自身，即给自己来个名字
// 那所谓的不动点，即 G(f) = f，称f的不动点，假设我有一个函数叫 Y
// Y 的用途即 对于任意给定 G，返回他的不动点 f
// 所以一般的递归函数可以表示为 f = G(YG)，但稍微绕个弯，其实就能表示成 f = YG





