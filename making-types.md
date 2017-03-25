# 타입과 타입클래스 만들기

## 대수 데이터 타입

지금까지 `Bool`, `Int`, `Char`, `Maybe` 등 다양한 타입을 배워왔습니다. 여기서 이런 타입들을 직접 만들어보겠습니다. 타입을 정의하기 위한 방법으로 **data** 키워드를 제공합니다. 표준 라이브러리에서 `Bool` 타입의 정의를 보면 아래와 같습니다. 

```haskell
data Bool = False | True
```

`data`는 새로운 타입을 정의한다는 것을 의미합니다. `=` 이전에 타입 이름을 표시하고, 나머지는 값 생성자들을(**value constructors**) 정의합니다. `|`은 _or_를 의미합니다. 따라서 `Bool` 타입은 `True`나 `False` 값을 가질 수 있습니다. 여기서 타입의 이름과 값들의 첫글자는 대문자입니다. 

유사한 방식으로 `Int` 타입의 정의는 아래와 같습니다. 

```haskell
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
```

첫번째와 마지막 값 생성자는 `Int` 타입이 가질 수 있는 최소값과 최대값입니다. 실제로 이렇게 정의되어 있는 것은 아니고, 설명을 위해 생략하였습니다. 

이번에는 하스켈로 모양을 표현해보겠습니다. 한가지 방법으로 튜플을 활용할 수 있습니다. 예를들어 원을 `(43.1, 55.0, 10.4)`로 표시한다면, 첫번째, 두번째 값은 원의 중심이고 세번째 값은 반지름이라고 할 수 있습니다. 다른 방법으로 3D 벡터 등으로 표현될 수 있지만, 가장 좋은 방법은 모양을 타입으로 만드는 것입니다. 모양을 타입으로 정의하면 아래와 같습니다. 

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

`Circle`의 생성자는 부동 소수점 필드 세개를 가지고 있습니다. 따라서 타입의 값 생성자를 작성할때 뒤에 타입을 추가할 수 있고, 각 타입은 포함할 값을 정의합니다. `Rectangle` 값 생성자는 4개의 부동 소수점을 받습니다. 앞의 2개는 왼쪽 뒤의 좌표를 나머지 2개는 오른쪽 아래의 좌표를 나타냅니다. 

**타입 정의에서 필드는 실제로 파라메터**이고, **값 생성자는 궁극적으로는 타입의 값을 리턴하는 함수**입니다. 위에서 정의한 타입의 타입 선언은 아래와 같습니다. 

```haskell
ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

아래 예제는 Shape을 받아서 Shape의 표면적을 리턴하는 함수입니다.

```
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 -x1) * (abs $ y2 - y1)
```

이 예제에서 가장 주목할 만한 것은 타입 선언인데, Shape을 받아서 Float를 리턴하는 함수입니다. `Circle`은 `Shape`처럼 타입이 아니기 때문에 `Circle -> Float`와 같이 선언할 수 없습니다. 마찬가지로 `True -> Int`와 같이 선언할 수 없습니다. 또한 위 예제에서 생성자에 의한 패턴매칭을 한 것을 확인할 수 있습니다. 첫번째 생성자 패턴매칭은 앞의 두 인자는 상관하지않고 세번째 인자인 반지름(radius)만 사용하였습니다. 

![](/assets/스크린샷 2017-03-25 오후 9.37.05.png)

위와같이 실행하면 패턴매칭에 의해서 정상적으로 동작하는 것을 확인할 수 있습니다. 하지만 `Circle 10 20 5`와 같이 실행하면 하스켈은 데이터 타입을 어떻게 문자열로 출력할지 모르기 때문에 에러가 발생합니다. 하스켈에서는 값을 프롬프트에 문자열로 출력하기 위해서 먼저 `show` 함수를 실행하고 터미널로 출력합니다. `Shape` 타입이 `Show` 타입클래스에 속하려면 아래와 같이 수정해야합니다. 

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
```

위와같이 _data_ 선언의 마지막에 `deriving (Show)`를 추가하면 타입은 `Show` 타입클래스에 속하게 됩니다. 이제 아래와 같이 사용이 가능해집니다. 

![](/assets/스크린샷 2017-03-25 오후 9.49.00.png)

값 생성자는 함수입니다. 따라서 다른 함수들처럼 map을 쓰거나 부분적으로 적용하는 등의 모든 것이 가능합니다. 만약 원의 중심은 같은데 반지름이 다른 원들의 리스트를 만드려면 아래와 같이 할 수 있습니다. 

![](/assets/스크린샷 2017-03-25 오후 10.00.31.png)

2차원 공간의 점을 부분 데이터 타입으로 선언하여 재활용할 수 있습니다. 타입을 분리하면 아래와 같이 `Shape`을 더 이해하기 쉽게 정의할 수 있습니다. 

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

`Point`를 선언할때 타입과 값 생성자에 동일하게 `Point`를 이름을 사용하였습니다. 이렇게 사용하는 것은 특별한 의미는 없지만, 일반적으로 타입에 한개의 값 생성자가 있을때 동일한 이름을 사용합니다. `Point`의 활용으로 `Circle`과 `Rectangle`이 좀 더 이해하기 쉽게 정의되었습니다. 이에따라서 `surface` 함수도 아래와 같이 재정의 됩니다. 

```haskell
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
``` 

여기서는 `Circle` 패턴은 `Point` 입력을 무시하였습니다. `Rectangle` 패턴에서는 `Point`의 값을 얻기위해서 중첩된 패턴매칭을 사용하였습니다. 이렇게 `Point` 자체의 값을 받아올때 as 패턴을 사용할수도 있습니다. 

![](/assets/스크린샷 2017-03-26 오전 12.09.29.png)


도형을 조금씩 움직이는 함수를 만들어 보겠습니다. 이 함수는 도형과 x축, y축으로 얼마나 이동할지를 받아서 같은 이차원에 새로운 도형을 리턴합니다. 

```haskell
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
```

이 예제에서는 직접적으로 도형의 위치에서 움직이는 양만큼 더해주었습니다. 

![](/assets/스크린샷 2017-03-26 오전 12.24.44.png)

만약 점을 직접적으로 다루고 싶지않다면 원점에서 어떤 크기의 도형을 만들고 조금씩 움직이는 보조 함수를 만들 수 있습니다. 

```haskell
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
```

![](/assets/스크린샷 2017-03-26 오전 12.30.24.png)

직접 정의한 데이터 타입은 모듈에서 노출시킬 수 있습니다. 모듈을 정의할때 노출시키는 함수와 함께 타입을 적어주고 괄호와 내부에 노출시킬 값 생성자를 콤마로 구분하여 명시하면 됩니다. 모든 값 생성자들을 노출시키려면 괄호안에 `..`를 넣어주면 됩니다. 

```haskell
module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  
```  

`Shape(..)`은 `Shape`의 모든 값 생성자들을 노출시켜 어떤 모듈이든지 `Rectangle`과 `Circle` 값 생성자들을 사용하여 도형을 만들 수 있게합니다. 즉, `Shape (Rectangle, Circle)`과 동일합니다. 

또한 `Shape`만 적어서 어떤 값 생성자도 노출시키지 않을 수도 있습니다. 이렇게하면 외부 모듈에서 `Shapes` 모듈을 import해서 `baseCircle`과 `baseRect`를 사용하여 도형들을 만들수 있습니다. 예를들면 `Data.Map`도 값 생성자를 노출하지 않았기 때문에 `Map.Map [(1,2),(3,4)]`과 같이 맵을 생성할 수 없습니다. 하지만 `Map.fromList`와 같은 보조함수를 사용하여 맵을 만들 수 있습니다. 여기서 **값 생성자도 결국 파라메터로 필드를 입력받아서 어떤 타입의 값(`Shape`과 같은)을 결과로 리턴하는 함수라는 점을 기억해야 합니다.** 따라서 노출하지 않으면 모듈을 가져온 곳에서 함수를 사용하는 것을 막을 수 있습니다. 그러나 노출된 다른 함수가 타입을 리턴하면, 이 함수를 통해서 타입의 값을 만들 수 있습니다.   

데이터 타입의 값 생성자를 노출하지 않으면 구현부를 숨겨서 추성화하고, 모듈의 사용자는 값 생성자와의 패턴매칭을 할 수 없습니다. 

## Record 문법

이제부터는 사람을 설명하는 데이터 타입을 만들어 보겠습니다. 사람에 대한 정보는 성, 이름, 나이, 키, 전화번호와 좋아하는 아이스크림 맛이 있습니다. 

```haskell
data Person = Person String String Int Float String String deriving (Show)
```

위와같이 `Person` 타입을 정의하고 사람을 만들어 보겠습니다. 

![](/assets/스크린샷 2017-03-26 오전 1.11.59.png)

가독성이 좋지는 않지만 사람을 만들었습니다. 만약 사람의 각 속성을 받아오는 함수를 만드려면 어떻게 할까요? 

```haskell
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor
```

위와 같이 작성하는게 고통스럽긴 하지만.. 이 메서드들은 잘 동작합니다. 

![](/assets/스크린샷 2017-03-26 오전 1.18.52.png)
 
하스켈를 만든사람은 이런한 상황을 만들지 않기위해서 Record를 만들었습니다.  
   
```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)
```    

각 필드의 타입 이름만 공백으로 구분하여 나열하는 대신 괄호를 사용하엿습니다. 먼저 `firstName`과 같이 필드명을 적고 `::`를 적고 타입을 명시합니다. 이 기능의 주요 이점은 데이터 타입에서 필드를 조회하는 함수를 만드는 것입니다. 하스켈에서는 자동으로 `firstName`, `lastName`, `age`, `height`, `phoneNumber`, `flavor` 함수를 만들어줍니다. 

![](/assets/스크린샷 2017-03-26 오전 1.28.39.png)

     
     
     
     
     
      
       
         
   
