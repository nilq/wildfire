# wildfire
A partly functional language running on LLVM

Syntax ideas
---

```fire
# simple recursive fibbonacci approach

func fib n : int -> int
  if n < 3
    pass 1                # explicit return
  end
  
  (fib n - 1) + fib n - 2 # implicit return
end
```

```fire
# ffi bindings
summon sin a : float -> float
summon cos a : float -> float

func circle_point a : float -> radius : int -> (float, float)
  ((radius * cos a), radius * sin a)
end
```
