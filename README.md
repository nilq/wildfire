# wildfire
A partly functional language running on LLVM

Syntax approach
---

```fire
# this is a comment, working v well 10/10
# hello

# function import test
summon print_char x

func fib x:
  if 3 > x:
    1
  else
    fib(x - 1) + fib(x - 2)
  end
end

func main:
  using a = 10, b = 2:
    for i = 1, i < a, b:
      print_char(fib(i))
    end
  end
end

```
