# Maybe 모나드 활용 예제

이전 절에서 배운 `Monad` 타입클래스를 활용하여 `Maybe`를 `Monad`의 인스턴스로 만들면 아래와 같다. 

```haskell
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing
```

`return`은 `Applicative`의 `pure`와 동일하게, 입력값을 `Just`에 래핑한다. 

`>>=` 함수는 `applyMaybe`와 동일하다. 함수에 `Maybe a`를 넣었을때, 왼쪽 입력이 `Nothing`이면 그대로 `Nothing`을 반환하고, `Just`이면 안에 있는 값을 꺼내서 `f` 함수에 적용한다. 

이제 작성된 `Maybe` 모나드를 실행해보면 아래와 같다. 

```haskell
ghci> return "WHAT" :: Maybe String  
Just "WHAT"  
ghci> Just 9 >>= \x -> return (x*10)  
Just 90  
ghci> Nothing >>= \x -> return (x*10)  
Nothing
```

우리가 어떻게 `/x -> return (x * 10)`에 `Just 9`를 입력했을때, 함수 내부의 `x`는 값 `9`를 얻도록 했는지 기억하시길 바랍니다. `>>=`를 활용하면 패턴매칭없이도 `Maybe`로 부터 값을 추출할 수 있는 것처럼 보입니다. 그러면서도 `Maybe`의 컨텍스트를 잃지 않았습니다. 

이제부터는 예제를 통해서 `>>=`를 활용해 여러가지 `Maybe a` 값들을 다루는 방법을 알아보겠습니다. 

피에르는 생선 농장에서 일을 마치고 줄 위를 걷는 놀이를 하려고 합니다. 피에르는 이 놀이에 익숙한 편이지만 한가지 문제가 있습니다. 균형을 잡기 위한 막대위에 새들이 앉아서 균형 잡는 것을 방해한다는 점입니다. 막대의 왼쪽의 새의 수가 오른쪽의 새의 수와 항상 같으면 크게 문제되진 않을 것입니다. 하지만 때때로 한쪽 편에 더 많은 수의 새가 앉아서 줄에서 떨어지곤 합니다. 

여기서 피에르는 막대의 왼쪽과 오른쪽 새의 수의 차이가 3마리 이내이면 균형을 유지할 수 있다고 가정하겠습니다. 예를들어 왼쪽에 4마리, 오른쪽에 1마리가 있다면 괜찮습니다. 하지만 왼쪽에 한마리가 더 앉으면 균형을 잃고 떨어지게 됩니다. 

이제 특정 수의 새들이 막대에 앉거나 날아갈때, 피에르가 잘 버티고 있는지 시뮬레이팅 할 것입니다. 예를들어, 첫번째 새가 왼쪽에 앉고나서 4마리의 새가 오른쪽에 않고, 왼쪽에 있던 새가 날아가면 피에르에게 어떤 일이 일어나는지 알아보고 싶습니다. 

막대에 앉은 새는 간단하게 정수의 쌍으로 표현할 것입니다. 튜플의 첫번째 값은 왼쪽에 앉은 새의 수이고, 오른쪽 값은 오른쪽에 앉은 새의 수입니다. 

```haskell
type Birds = Int  
type Pole = (Birds,Birds)
```

먼저 `Int`의 타입동의어로 `Birds`, `(Birds, Birds)`의 타입동의어로 `Pole`을 만들었습니다. 다음으로 막대의 각 위치에 앉거나 날아간 새의 수와 현재 막대를 받아서, 새로운 막대를 반환하는 함수를 만들면 아래와 같습니다. 

```haskell
landLeft :: Birds -> Pole -> Pole  
landLeft n (left,right) = (left + n,right)  
  
landRight :: Birds -> Pole -> Pole  
landRight n (left,right) = (left,right + n) 
```

```haskell
ghci> landLeft 2 (0,0)  
(2,0)  
ghci> landRight 1 (1,2)  
(1,3)  
ghci> landRight (-1) (1,2)  
(1,1)
```

막대에 앉은 새는 양수로 날아간 새는 음수로 표현했습니다. 두 함수 모두 `Pole`을 반환하기 때문에 아래와 같이 체이닝할 수 있습니다. 

```haskell
ghci> landLeft 2 (landRight 1 (landLeft 1 (0,0)))  
(3,1)
```

`landLeft 1`에 `(0, 0)`을 적용하면, `(1, 0)`이 되고, 오른쪽에 새 한마리가 앉아서 `(1, 1)`이 됩니다. 마지막으로 왼쪽에 새 두마리가 앉아서 `(3, 1)`이 됩니다. 여기서는 먼저 함수를 쓰고 매개변수를 작성하지만, 이 예제에서는 먼저 막대의 상태가 있고 함수가 오는 것이 더 말이됩니다. 이것을 함수로 작성하면 아래와 같습니다. 

```haskell
x -: f = f x 
```

이 함수를 사용하면 먼저 함수의 매개변수, 함수 순으로 호출할 수 있습니다. 

```haskell
ghci> 100 -: (*3)  
300  
ghci> True -: not  
False  
ghci> (0,0) -: landLeft 2  
(2,0)
```

이 함수를 사용해서 다시 막대에 새가 앉는 상황을 체이닝하면 아래와 같이됩니다. 

```haskell
ghci> (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2  
(3,1)
```

이전에 봤던 예제와 동작을 동일하지만, 초기 막대의 상태인 `(0, 0)`에서 막대의 상태 변화가 훨씬 명확하게 보입니다. 

지금까지는 피에르가 균형을 유지할 수 있지만, 아래와 같은 상황에서는 어떻게 될까요? 

```haskell
ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)
```

진행 과정 중간에 `(4, 0)`인 막대가 되는 단계에서 피에르는 떨어질 것입니다. 하지만, 위 예제에서는 마치 문제가 없었던 것처럼 `(0, 2)`를 반환했습니다. 이 문제를 수정하려면 `landLeft`와 `landRight` 함수를 수정해야 합니다. 위 예제를 보면 이 함수들에 실패가 필요해보입니다. 즉, 균형에 문제가 없으면 새로운 막대를 반환하길 원하지만, 문제가 생기면 실패를 알려주길 원합니다. 이런 컨텍스트가 바로 `Maybe`입니다. 이제 `landLeft`와 `landRight` 함수를 `Maybe`를 사용해서 재작성 하겠습니다.

```haskell
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  
```



















