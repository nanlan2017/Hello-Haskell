#<center><font color=blue> 命令式思路  

 >>>>>>只要我能用命令式的伪代码写出来，就肯定可以对应成 Haskell 步骤！

---


### <center><font color=blue> 操作 List时要时刻想到 递归
### List操作：几乎总存在recursive 的算法
设想：如果有一个 元素 和 已经 operated 好的list ，能不能 合成结果 ？？  
如果可以，就完全可以化成  
```
    operate (x:xs) =  x ### (operate xs) 
```
### <center><font color=blue size=4> we need a utility type because lists in haskell are homogeneous
### <center><font color=blue size=4> Haskell 中的 List 要求其中元素为同一类型: 这一点要在任何操作函数的 类型签名中固定
- 因此：若要操作层层嵌套的 List，如
  > [1, [2,[3,4],5]]
  
  需要自定义一个类型，才能<font color=red>__用同一个类型代表 1， [3,4] 这两种形式的elements__

   