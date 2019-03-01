#<font color=red> 命令式思路  

 >>>>>>只要我能用命令式的伪代码写出来，就肯定可以对应成 Haskell 步骤！

---


### <font color=blue> 操作 List时要时刻想到 递归
### Rule 1
 List操作：几乎总存在recursive 的算法
> 设想：如果有一个 元素 和 已经 operated 好的list ，能不能 合成结果 ？？  
如果可以，就完全可以化成  
```
    operate (x:xs) =  x ### (operate xs) 
```

### Rule 2
 List操作：当变换是一个元素的粒度时，百分百是用map
> 有些变换完全就是基于一个element 的变换（没有多个elements 之间互相影响）


### <center><font color=blue size=4> we need a utility type because lists in haskell are homogeneous
### <center><font color=blue size=4> Haskell 中的 List 要求其中元素为同一类型: 这一点要在任何操作函数的 类型签名中固定
- 因此：若要操作层层嵌套的 List，如
  > [1, [2,[3,4],5]]
  
  需要自定义一个类型，才能<font color=red>__用同一个类型代表 1， [3,4] 这两种形式的elements__

---   

### <font color=red> 要耐心：把计算的过程 分解成可想到的步骤
### 就算一下子解决不了，也可以一步一步进行靠近、得到点东西。
### 问题就被分解成几个简单的问题了

按其长度出现的频率：
lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]  --长度4的只出现了一个，故把长度4的排前面
-}

lsort :: [String] -> [String]
lsort = sortBy (\x y -> compare (length x) (length y))

-- 先求所有的长度值的集合：   lengthSum = [1,2,4]
sort (按大小)  . removeDuplicate .  map length $ list
-- 再对其中的每一个长度进行filter   
map (\x-> filter (length)) lengthSum
