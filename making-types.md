# 사용자 정의 타입 만들기

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

**타입 정의에서 필드는 실제로 매개변수**이고, **값 생성자는 궁극적으로는 타입의 값을 리턴하는 함수**입니다. 위에서 정의한 타입의 타입 선언은 아래와 같습니다. 

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

이 예제에서 가장 주목할 만한 것은 타입 선언인데, Shape을 받아서 Float를 리턴하는 함수입니다. `Circle`은 `Shape`처럼 타입이 아니기 때문에 `Circle -> Float`와 같이 선언할 수 없습니다. 마찬가지로 `True -> Int`와 같이 선언할 수 없습니다. 또한 위 예제에서 생성자에 의한 패턴매칭을 한 것을 확인할 수 있습니다. 첫번째 생성자 패턴매칭은 앞의 두 매개변수는 상관하지않고 세번째 매개변수인 반지름(radius)만 사용하였습니다. 

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

`Point`를 선언할때 타입과 값 생성자에 동일하게 `Point`를 이름을 사용하였습니다. 이렇게 사용하는 것은 특별한 의미는 없지만, 일반적으로 타입에 한개의 값 생성자가 있을때 동일한 이름을 사용합니다. `Point`의 활용으로 `Circle`과 `Rectangle`이 좀 더 이해하기 쉽게 정의되었습니다. 이에 따라서 `surface` 함수도 아래와 같이 재정의 됩니다. 

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

`Shape(..)`은 `Shape`의 모든 값생성자들을 노출시켜 어떤 모듈이든지 `Rectangle`과 `Circle` 값 생성자들을 사용하여 도형을 만들 수 있게합니다. 즉, `Shape (Rectangle, Circle)`과 동일합니다. 

또한 `Shape`만 적어서 어떤 값 생성자도 노출시키지 않을 수도 있습니다. 이렇게하면 외부 모듈에서 `Shapes` 모듈을 import해서 `baseCircle`과 `baseRect`를 사용하여 도형들을 만들수 있습니다. 예를들면 `Data.Map`도 값생성자를 노출하지 않았기 때문에 `Map.Map [(1,2),(3,4)]`과 같이 맵을 생성할 수 없습니다. 하지만 `Map.fromList`와 같은 보조함수를 사용하여 맵을 만들 수 있습니다. 여기서 **값생성자도 결국 매개변수로 필드를 입력받아서 어떤 타입의 값(`Shape`과 같은)을 결과로 리턴하는 함수라는 점을 기억해야 합니다.** 따라서 노출하지 않으면 모듈을 가져온 곳에서 함수를 사용하는 것을 막을 수 있습니다. 그러나 노출된 다른 함수가 타입을 리턴하면, 이 함수를 통해서 타입의 값을 만들 수 있습니다.   

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

또다른 이점은 타입에 `Show`를 사용했을때, Record를 사용하면 화면에 다른게 출력된다는 점입니다. 

차를 만든 회사, 모델이름, 생산연도를 속성으로 가지는 자동차를 정의해보겠습니다. 

```haskell
data Car = Car String String Int deriving (Show)
```

![](/assets/스크린샷 2017-03-26 오전 1.34.23.png)
              
이것을 Record 문법을 사용해서 재정의하면 아래와 같습니다. 

```haskell
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
```          
     
![](/assets/스크린샷 2017-03-26 오전 1.35.28.png)     
     
이 예제에서 자동차를 하나 만들었습니다. 위와 같이 작성하면 생성할때 필드의 순서를 지켜서 작성할 필요가 없습니다. 만약 Record를 사용하지 않으면 반드시 순서대로 인자들을 입력해야 합니다. 

**생성자에 여러 필드가 있고 명확하지 않은 경우, Record 문법을 사용합니다.** 만약 3D 벡터 타입을 만든다면, `data Vector = Vector Int Int Int`와 같이 만들 수 있습니다. 이 경우는 벡터를 구성하는 필드들이 명확합니다. 하지만 `Person`, `Car` 타입은 필드들의 타입만으로는 명확하지 않습니다. 

## 타입 매개변수
       
값 생성자는 어떤 값 매개변수를 받아서 새로운 값을 생성합니다. 예를들어 `Car` 생성자는 3개의 값을 받아서 차를 생성합니다. 유사한 방식으로 **타입 생성자(type constructors)는 새로운 타입을 생성하기 위해서 매개변수로서 타입을 받을 수 있습니다.** 처음에는 약간 어렵게 들리지만 그렇게 복잡하지 않습니다. 만약 C++의 템플릿에 익숙하다면 몇가지 유사점을 볼 수 있습니다. 

```haskell
data Maybe a = Nothing | Just a
```                

여기서 `a`는 타입 매개변수입니다. 그리고 타입 매개변수를 포함하고 있는 `Maybe`를 타입 생성자라고 부릅니다. 데이터 타입이 `Nothing`이 아닐때는 가지고 있는 타입에 따라서 타입 생성자는 `Maybe Int`, `Maybe Car`, `Maybe String` 등의 타입을 생성할 수 있습니다. 이것은 타입이 아니라 타입 생성자이기 때문에, 값이 없어도 그냥 `Maybe`로 타입을 가질 수 있습니다. 이것이 값의 일부가 될 수 있는 실제 타입이 되려면 모든 타입 매개변수가 채워져야 합니다.  

만약 `Maybe`에 타입 매개변수로 `Char`를 넘기면, `Maybe Char` 타입을 얻을 수 있습니다. 값 `Just 'a'`는 `Maybe Char` 타입을 가집니다. 

`Maybe`를 사용하기전에 우리는 이미 타입 매개변수를 가진 타입을 사용해왔습니다. 바로 리스트 타입입니다. 리스트 타입은 구체적인 타입을 생성하기 위해서 타입 매개변수를 사용합니다. 리스트의 값들은 `[Int]`타입, `[Char]`타입, `[[String]]`타입을 가질 수 있지만, 타입이 `[]`뿐인 값을 가질 수는 없습니다.      
      
![](/assets/스크린샷 2017-03-26 오전 2.50.05.png)            
               
타입 매개변수를 사용하면 데이터 타입에 담기를 원하는 타입에 따라서 여러가지 타입으로 만들 수 있습니다. `:t Just "Haha"`를 수행했을때, `Just a`의 `a`가 문자열이면 `Maybe a`의 `a`도 문자열이기때문에 `Maybe [Char]`로 타입 추론되었습니다. 

`Nothing`의 타입은 `Maybe a`입니다. 만약 `Maybe Int`를 매개변수로 받는 함수가 있다면, `Nothing`을 입력으로 넣을 수 있습니다. 왜냐하면 `Nothing`은 어떤 값도 포함하고 있지않고 어떤 값이든 상관하지 않기 때문입니다. `5`가 `Int`나 `Double`로 동작할 수 있는 것 처럼, `Maybe a` 타입은 `Maybe Int`처럼 동작할 수 있습니다. 유사하게 빈리스트의 타입은 `[a]`가 됩니다. 따라서 빈리스트는 어떤 리스트든 될 수 있습니다. 이런 이유로 `[1,2,3] ++ []`와 `["ha","ha","ha"] ++ []`가 가능합니다. 
 
타입 매개변수를 사용하는 것은 이점이 많지만, 적절하게 사용되어야 합니다. 일반적으로 `Maybe a` 타입처럼 데이터 타입이 보유하는 값의 타입에 관계없이 동작할때 사용합니다. 마치 타입이 일종의 박스처럼 사용될때 적합합니다. `Car`의 데이터 타입을 아래와 같이 변경할 수 있습니다. 

```haskell
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)
```

를 아래와 같이 변경할 수 있습니다. 

```haskell
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)
```  
    
하지만 위와같이 변경하는 것에 이점이 있을까요? 정답은 아마도 없다일 것입니다. 왜냐하면 `Car String String Int`에서만 동작하는 함수를 정의했기 때문입니다. 예를들어 `Car`의 첫번째 정의에서는 자동차의 속성을 작은 텍스트로 표시하는 함수를 만들 수 있습니다. 

```haskell
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```                                                                                    
![](/assets/스크린샷 2017-03-26 오후 11.15.13.png)

만약 `Car`의 두번째 정의인 `Car a b c`라면 어떻게 될까요?

```haskell
tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
```

이때는 `Car`의 타입을 `(Show a) => Car String String a`로 지정해야 합니다. 따라서 타입 선언이 더 복잡해졌습니다. 유일하게 얻을 수 있는 이점은 `c`의 타입으로 `Show` 타입클래스의 인스턴스인 어떤 타입이든 사용할 수 있다는 것입니다.  

![](/assets/스크린샷 2017-03-26 오후 11.26.34.png)

실제상황에서는 `Car String String Int`로 사용할 것이고, `Car`의 타입을 매개변수화 하는 것은 가치가 없어 보입니다. **일반적으로 데이터 타입의 다양한 값 생성자 안에 포함된 타입이 타입이 동작하는데 중요하지 않은 경우, 타입 매개변수를 사용합니다.** 물건의 리스트는 물건의 리스트이고 물건의 종류에 관계없이 동작할 수 있습니다. 숫자들의 리스트의 합을 구한다면, 합계를 구하는 함수에서 나중에 숫자들의 리스트라는 것을 지정할 수 있습니다. `Maybe`에서도 동일합니다. `Maybe`는 아무것도 가지고 있지않거나 어떤 것을 가지고 있는 것을 의미합니다. 어떤 것이 무슨 타입이든 상관하지 않습니다.   

타입 매개변수의 또다른 예로 `Data.Map`의 `Map k v`가 있습니다. `k`는 맵의 키의 타입이고, `v`는 값들의 타입입니다. 맵의 예는 타입 매개변수를 매우 유용하게 사용하는 좋은 예입니다. 매개변수화된 맵을 사용하면 키의 타입이 `Ord` 타입클래스에 포함되는한 어떤 타입이든 다른 타입으로의 맵핑이 가능합니다. 이러한 맵핑 타입을 정의하려면, _data_ 선언에 타입클래스 제약조건을 추가할 수 있습니다. 

```haskell
data (Ord k) => Map k v = ...
```  

하지만 하스켈에서는 **절대 data 선언에 타입클래스 제한자를 추가하지 말라**는 매우 강력한 관습이 있습니다. 왜냐하면 별다른 이점이 없을때나 필요없을때도 더 많은 클래스 제약조건을 추가하기 때문입니다. `Map k v` _data_ 선언에서 `Ord k` 제약조건을 넣으면, 맵에서 키들이 정렬될 수 있다고 가정하는 함수들 안에 제약조건을 넣어야 합니다. 하지만 _data_ 선언에 제약조건을 넣지않으면, 키들의 정렬 여부에 관계없이 함수의 타입 선언안에 `(Ord k) =>`를 넣지않아도 됩니다. 이런 함수의 예로 `toList`가 있는데, 맵핑 함수를 받아서 관련된 리스트로 변환합니다. 이 함수의 타입 선언은 `toList :: Map k a -> [(k, a)]`입니다. 만약 `Map k v`가 _data_ 선언에서 타입 제약조건을 가지고 있었다면, `toList`의 타입 선언은 순서에 따라서 키를 비교하지 않음에도 불구하고 `toList :: (Ord k) => Map k a -> [(k, a)]`가 되어야 합니다.  

따라서 **_data_ 선언내에는 타입 제약조건이 적절하더라도 넣지 않아야 합니다. 왜냐하면 함수의 타입 선언에도 필요여부에 관계없이 제약조건을 넣어야 하기 때문입니다.** 

여기서는 3D 벡터 타입을 몇가지 함수와 함께 구현해보도록 하겠습니다. 일반적으로 숫자 타입을 포함하지만 여전히 여러가지 타입을 지원하기 때문에 매개변수화된 타입을 사용할 것입니다. 

```haskell
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector 1 m n) = Vector (i+1) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector 1 m n) = i*1 + j*m + k*n
```

`vplus`는 두개의 벡터를 더합니다. `scalarMult`는 두 벡터의 스칼라 곱이고, `vectMult`는 벡터에 스칼라는 곱하는 것입니다. 이 함수들은 `Vector Int`, `Vector Integer`, `Vector Float` 등 `Vector a`의 `a`가 `Num` 타입클래스에 포함되는한 어떤 타입이든지 동작할 수 있습니다. 또한 함수에 대한 타입 선언을 살펴보면 동일한 타입의 벡터에서만 동작할 수 있고 숫자들은 벡터에 포함된 타입이어야 합니다. 이 예제에서는 함수들에는 어차피 반복적으로 제약조건을 넣어야 하기때문에 _data_ 선언에서 `Num` 타입클래스에 대한 제약조건을 넣지않았습니다. 

다시한번 **타입 생성자와 값 생성자를 구분하는 것은 매우 중요합니다.** 데이터 타입을 선언했을때, `=`전 부분이 타입 생성자이고, 나머지 부분의 생성자들이 값 생성자 입니다.(`|`로 구분된) `Vector t t t -> Vector t t t -> t`와 같은 함수는 잘못된 것입니다. 왜냐하면 타입 선언안에 타입들을 넣어야하고 벡터 타입 생성자는 하나의 매개변수만 사용하기 때문입니다. 반면에 값 생성자는 세개를 받습니다.  

![](/assets/스크린샷 2017-03-27 오전 1.17.13.png)

## 파생된 인스턴스(Derived instances)

이전 챕터에서 타입클래스는 어떤 행동을 정의하는 인터페이스의 일종이다라고 설명했습니다. 타입은 어떤 행위를 가진 타입클래스의 인스턴스가 될 수 있습니다. 예를들어 `Int` 타입은 `Eq` 타입클래스의 인스턴스입니다. 왜냐하면 `Eq` 타입클래스는 같아질 수 있는 것에 대한 행위를 정의합니다. 정수는 동일한 값을 가질 수 있으므로 `Int`는 `Eq` 타입클래스에 속합니다. 실제로 `Eq`의 인터페이스의 행위로는 `==`와 `=/` 함수가 함께 제공됩니다. 만약 타입이 `Eq` 타입클래스에 속한다면, 타입의 값들로 `==` 함수를 사용할 수 있습니다. 예를들어 `4 == 4`와 `"foo" /= "bar"` 표현할 수 잇습니다. 

타입클래스는 Java, Python, C++ 언어의 클래스와 종종 혼돈될 수도 있습니다. 이런 언어들의 클래스는 속성과 동작을 포함한 객체를 생성하는 뼈대입니다. 타입클래스는 인터페이스에 더 가까워서 타입클래스로부터 데이터를 만들지 않습니다. 대신 데이터 타입을 먼저 만들고 어떤 동작을 할 수 있을지 생각합니다. 만약 동일해질 수 있다면 `Eq` 타입클래스의 인스턴스로 만듭니다. 만약 순서를 정할 수 있다면 `Ord` 타입클래스의 인스턴스로 만듭니다. 

다음 섹션에서 어떻게 타입클래스에 정의된 함수를 구현해서 우리만의 타입클래스의 타입 인스턴스를 만들 수 있을지를 살펴볼 것입니다. 하지만 지금은 어떻게 하스켈이 자동으로 `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read` 타입클래스 중 하나의 타입을 인스턴스로 만드는지 확인해보겠습니다. 하스켈은 만약 _data_ 타입을 만들때 _deriving_ 키워드를 사용한다면, 우리의 타입의 동작을 유도해 낼 수 있습니다. 
 
```haskell
data Person = Person { firstName :: String 
                     , lastName :: String
                     , age :: Int
                     }
```
        
사람을 표현하는 데이터 입니다. 두 사람의 성,이름, 나이의 조합이 같은 경우는 없다고 가정하겠습니다. 두 사람에 대한 Record를 가지고 있다면, 두 사람이 같은지 다른지 확인할 수 있습니다. 따라서 `Person` 타입은 `Eq` 타입클래스에 속하는게 적합합니다. 

```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq)  
``` 

위 예제와같이 타입을 `Eq`로 derive했을때 두값을 `==`이나 `/=`로 `Person` 타입의 두 값을 배교해봅시다. 하스켈은 값 생성자가 매칭되면 `Person`안에 포함된 모든 값의 쌍을 `==`으로 테스트합니다. `Person` 타입이 `Eq` 타입클래스에 속하면 `Person`이 가진은 모든 필드의 타입도 `Eq` 타입클래스에 속해야 합니다. 따라서 `String`과 `Int` 모두 `Eq` 타입클래스에 속합니다. 

![](/assets/스크린샷 2017-03-28 오전 2.04.59.png)

`Person`은 현재 `Eq`에 속하기 때문에, 타입 선언안에 `Eq a` 클래스 제약조건에서 `Person`을 `elem`과 같은 모든 함수들을 위한 `a`로 사용할 수 있습니다.  

![](/assets/스크린샷 2017-03-28 오전 2.10.59.png)

`Show`와 `Read` 타입클래스는 각각 문자열로 또는 문자열로부터 변환될 수 있는 것들입니다. `Eq`와같이 타입 생성자가 필드를 가지고 있다면 타입을 인스턴스로 만들기 위해서 `Show` 또는 `Read`에 속해야 합니다. `Person` 데이터 타입을 `Show`와 `Read`에 속하도록 하려면 아래와 같이 합니다. 

```haskell
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)  
```

![](/assets/스크린샷 2017-03-28 오전 2.20.00.png)

여기서 `Person`을 `Show`에 속하는 객체로 만들지 않았다면 하스켈을 화면에 출력하는 방법을 몰라서 에러가 발생했을 것입니다. 하지만 위 예제에서는 `Show`에서 파생되었기 때문에 출력할 수 있습니다. 

`Read`는 `Show`와 반대입니다. `Show`는 값을 문자열로 변환하고, `Read`는 문자열을 값으로 변환합니다. `read` 함수를 사용할때는 아래와같이 변환하려는 타입을 명시해 주어야합니다. 

![](/assets/스크린샷 2017-03-31 오전 1.05.25.png)

`read`의 결과를 하스켈이 읽었을때 `Person`이라는 것을 추측할 수 있다면 아래와 같이 타입을 명시하지 않아도 됩니다. 

![](/assets/스크린샷 2017-03-31 오전 1.11.48.png)

매개변수화된 타입들을 읽을수도 있지만 타입 매개변수를 넣어야만 합니다. 따라서 `read "Just 't'" :: Maybe a`와 같이 작성할 수 없고, `read "Just 't'" :: Maybe Char`은 작성할 수 잇습니다. 

타입이 순서를 가지고 있을때 `Ord` 타입클래스의 인스턴스로 만들 수 있습니다. 서로 다른 생성자로 만들어진 동일한 타입의 두 값을 비교한다면, 타입에서 먼저 정의된 생성자로 만들어진 값이 더 작은 것입니다. 예를들어 `Bool` 타입은 `False`와 `True`값을 가질 수 있습니다.

```haskell
data Bool = False | True deriving (Ord)
```  

위 예제에서 `False` 값 생성자가 `True` 값 생성자보다 먼저 명시되었기 때문에 `True`는 `False`보다 큽니다. 

![](/assets/스크린샷 2017-03-31 오전 1.28.57.png)

`Maybe a` 데이터 타입은 `Nothing` 값 생성자가 `Just` 값 생성자전에 명시되어 있으므로 somthing이 아무리 작아도 `Nothing`은 항상 `Just somthing`보다 작습니다. 그러나 두개의 `Just`값을 비교하면 something으로 비교됩니다. 

![](/assets/스크린샷 2017-03-31 오전 1.40.09.png)

`Just (*3) > Just (*2)`은 `(*3)`과 `(*2)`가 함수라서 `Ord`의 인스턴스가 아니기 때문에 비교할 수 없습니다. 

   
`Enum`과 `Bounded` 타입클래스를 사용하면 쉽게 열거형 타입을 만들 수 있습니다. 

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```


