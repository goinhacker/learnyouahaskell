# 정적 타입 시스템

**하스켈은 정적 타입시스템을 사용합니다. 하스켈에서 모든 표현식\(expression\)의 타입은 컴파일 타임에 결정됩니다. **만약 코드 작성의 오류를 범하면 컴파일이 되지 않기때문에 좀 더 안전한 코드를 작성하는데 도움이 됩니다. 이것은 실행 타임에 크래쉬가 발생할 확률을 줄여주기도 합니다. 하스켈의 모든 것은 타입을 가지고 있기때문에 컴파일러는 컴파일전에 프로그램에 대한 매우 많은 것을 미리 알 수 있습니다.

자바나 파스칼과는 달리, **하스켈은 타입 추론을 합니다. **만약 숫자를 쓰는 중이라면 숫자라고 알려줄 필요가 없습니다. 스스로 추론할 수 있기 때문에 함수나 표현식의 타입을 명시적을 쓸 필요가 없습니다. 여기까지 하스켈의 타입 시스템에 대해서 아주 간단하게 알아봤지만, 하스켈을 배우는데 있어서 타입 시스템을 이해하는 것은 매우 중요합니다.

타입은 모든 표현식\(expression\)이 가지는 라벨과 같습니다. 타입은 표현식이 어떤 범주에 맞는지를 알려줍니다. \(True는 boolean, “Hello”는 문자열 등\)

지금부터 GHCI에서 :t 명령어를 사용해서 몇가지 올바른 표현식\(expression\)에 타입을 검사해보겠습니다.

![](https://cdn-images-1.medium.com/max/1600/1*hw3oPZRyfD4-wL8j2mEk5Q.png)

:t 명령어를 사용하면 해당 표현식에 ::\(has type of\)를 붙여서 타입을 알려주는 것을 알 수 있습니다. 타입은 항상 첫글자가 대문자 입니다. 이 예제에서 눈여서 살펴볼 것은 **“Hello”가 \[Char\]와 같이 문자의 리스트 타입을 되어있다는 것입니다. 리스트와는 다르게 튜플은 각 길이에 따라서 다른 타입을 가지는 것을 볼 수 있는데**, 아래 예제를 보면 좀 더 쉽게 이해할 수 있을 것입니다.

![](https://cdn-images-1.medium.com/max/1600/1*PNTUgIVZ-j_LNCD9suemXw.png)

**함수들도 타입을 가집니다. 우리가 함수를 작성할 때는 명시적으로 타입 선언을 선택할 수 있습니다. **일반적으로 함수를 작성할 때는 매우 짧은 함수를 제외하고는 타입 선언을 명시하는 것이 좋습니다.

![](https://cdn-images-1.medium.com/max/1600/1*IlsrO91PeyxgUaS3mtWRKA.png)

문자 리스트를 입력받아서 대문자만 남기고 모두 제거하는 문자 리스트를 반환하는 함수 입니다. 여기서 removeNonUppercase 함수는 \[Char\] -&gt; \[Char\] 타입을 가지고, 문자열을 입력 받아서 문자열을 리턴한다는 의미입니다. 여기서 \[Char\]은 문자열과 동일하기때문에 removeNonUppercase :: String -&gt; String으로 선언할 수도 있습니다.

**함수를 선언할때도 컴파일러가 추론이 가능하기 때문에 타입을 명시하지 않아도 됩니다. **하지만, 위 예제에서는 타입을 선언했습니다. 만약 여러개의 파라메터를 받는 함수를 선언한다면 어떻게 할까요?

![](https://cdn-images-1.medium.com/max/1600/1*r1kjjvPzhH42GT0c18xaHQ.png)

이 예제는 세개의 파라메터를 받아서 모두 더하는 함수 입니다. **다른 구분자 없이 -&gt; 로 파라메터를 구분하고 마지막에 리턴 타입을 적었습니다. **뒤에서 왜 리턴 타입과 파라메터를 구분하기 위해서 별도의 구분자를 사용하지 않아도 되는지 배울 것 입니다.

아래는 하스켈에서 사용하는 몇몇 공통 타입에 대한 설명입니다.

* **Int**
  는 integer를 나타냅니다. Int는 모든 숫자들에 사용됩니다. Int는 32bit 머신에서 보통 -2147483648 ~ 2147483647의 범위를 가집니다.
* **Integer**
  도 역시 integer를 나타냅니다. Int와의 차이점은 범위가 없다는 점이고, 매우 매우 큰 수를 표현할 때 사용됩니다. 하지만 Int에 비해서 비효율적으로 동작합니다.

![](https://cdn-images-1.medium.com/max/1600/1*3wyWIW9ExYLyqftAdBZ8kg.png)

![](https://cdn-images-1.medium.com/max/1600/1*b--1fioowREI4mJN09C4RQ.png)

* **Float**
  는 단일 정밀도를 가지는 부동 소수점입니다.

![](https://cdn-images-1.medium.com/max/1600/1*fRfNU6n_hXf6mPhmZxtmnA.png)

![](https://cdn-images-1.medium.com/max/1600/1*Ib7dFD_c5SrAGwdl1mfC3Q.png)

* **Double**
  는 두배의 정확성을 가지는 부동 소수점 입니다.

![](https://cdn-images-1.medium.com/max/1600/1*ecwfBUdpkjDfJE_rQ6cyig.png)

![](https://cdn-images-1.medium.com/max/1600/1*7o77HCVs87mDhBhP_hxrTA.png)

* **Bool**
  은 boolean 타입니다. True와 False만 가질 수 있습니다.
* **Char**
  은 문자를 나타냅니다. ‘’로 표현할 수 있고, 문자의 리스트는 문자열 입니다.
* **튜플**
  의 경우, 튜플내의 원소의 개수와 각 원소의 타입에 따라서 튜플의 타입이 전부 달라지기 때문에 무한대의 타입을 가진다고 볼 수 있습니다. 튜플은 \(\)로 표현할 수 있습니다.

# 타입 변수

먼저 head 함수의 타입에 대해서 살펴봅시다.

![](https://cdn-images-1.medium.com/max/1600/1*Tatln4J0PX5JZ5MaE0RitA.png)

head 함수는 어떤 타입의 리스트를 받아서 첫번째 요소를 반환하는 함수 입니다. 따라서 head의 타입은 \[a\] -&gt; a 인데 여기서 **“a”는 타입 변수\(type variable\) **입니다. 타입 변수 a는 마치 다른 언어들의 generics처럼 어떤 타입이든 될 수 있습니다. 하스켈에서는 이 타입 변수를 이용해서 매우 일반적인 함수들을 쉽게 작성할 수 있습니다. **타입 변수를 가진 함수들을 다형 함수\(polymorphic functions\)**라고 부릅니다. 결론적으로 head 함수는 어떤 타입의 리스트를 받아서 동일 타입의 리스트의 첫번째 요소를 반환하는 것을 알 수 있습니다.

타입 변수는 하나의 이상의 문자열로 이름을 정할 수 있지만 일반적으로 a,b,c,d.. 같은 것을 사용합니다.

![](https://cdn-images-1.medium.com/max/1600/1*t5OV20rNRtZleMVwXwp_jw.png)

fst 함수의 타입을 살펴보자. fst는 두개의 타입을 포함하는 튜플를 입력 받아서 첫번째 컴포넌트와 동일한 타입의 요소를 반환하는 함수입니다. 위 예제에서 a, b는 다른 타입 변수입니다. 여기서 a와 b는 같은 타입일 수 있습니다. 단, 반화하는 값의 타입은 튜플의 첫번째 컴포넌트의 타입과 같아야 합니다.

# 타입 클래스

타입 클래스는 어떤 행위를 정의하는 일종의 인터페이스 입니다. 타입 클래스는 OOP의 클래스와는 다르고, 오히려 Java의 인터페이스에 가까운 개념입니다. 

== 함수의 타입을 보면 아래와 같습니다.

![](https://cdn-images-1.medium.com/max/1600/1*NSEdbu5itJMgibGE32Fl7w.png)

> 하스켈에서는 비교 연산자 ==도 함수입니다. 마찬가지로 +, \*, -, / 등 연산자들도 함수 입니다. 만약 함수가 문자들로만 구성되어 있다면 기본적으로 infix로 사용됩니다. 타입을 체크할때나 다른 함수에 파라메터로 넘길때, 또는 prefix 함수를 사용할 때는 위와 같이 괄호로 묶어 줍니다.

이 예제에서는 타입에 “=&gt;”라는 새로운 심볼이 나온 것을 확인하실 수 있습니다. 여기서 **“=&gt;” 심볼 이전에 나온 모든 것은 클래스 제약\(class constraint\)**이라고 부릅니다. “\(==\) :: \(Eq a\) =&gt; a -&gt; a -&gt; Bool”를 해석하면, == 함수는 Eq 클래스의 멤버이고, 동일한 타입을 가진 두개의 값을 얻어서 Bool을 반환한다 입니다. 여기서 “=&gt;” 이전에 나온 **“\(Eq a\)”가 함수의 입력 a가 Eq 계열이어야 한다는 클래스 제약에 해당 **합니다.

Eq 타입클래스는 같은 값인지를 테스트하기 위한 인터페이스를 제공합니다. 어떤 타입의 두값이 같은지를 테스트하기 위해서는 두 값이 Eq 타입클래스 계열이어야 합니다. **하스켈에서는 IO를 제외한 모든 표준 타입과 함수들은 Eq 타입클래스 계열입니다.**

elem 함수는 내부적으로 ==을 사용하기 때문에 “\(Eq a\) =&gt; a -&gt; \[a\] -&gt; Bool” 타입을 가지고 있습니다.

몇가지 기본적인 타입클래스를 살펴보면 아래와 같습니다.

![](https://cdn-images-1.medium.com/max/1600/1*-dRhUAVpWq-ZNXZamCmDiA.png)

**Eq**는 같은 값인지 테스트하는 것을 지원하는 타입들에 사용됩니다. 멤버 함수로는 ==과 /= 함수가 있습니다. 따라서 어떤 함수내 타입 변수\(type variable\)에 Eq 클래스 제약이 있다면, 내부적으로 어디선가 ==또는 /=을 사용합니다. 따라서 이런 Eq계열의 타입들은 같은 값인지 비교가 가능합니다.

![](https://cdn-images-1.medium.com/max/1600/1*uHyhBIJtBRzwMrYiuCMkKg.png)

**Ord**는 순서를 가지는 타입들을 위한 것입니다.

![](https://cdn-images-1.medium.com/max/1600/1*fQwz51lbiJpduW9mIOplow.png)

Ord는 &gt;, &lt;, &gt;=, &lt;=와 같은 모든 비교 함수를 커버합니다. compare 함수는 두개의 같은 Ord 계열의 타입을 받아서 순서를 반환합니다. “Ordering”은 GT\(greater than\), LT\(lesser than\), EQ\(equal\)가 될 수 있는 타입입니다.

여기서 Ord 계열이 되기 위해서는 먼저 Eq 계열이어야 합니다.

![](https://cdn-images-1.medium.com/max/1600/1*caUqObgcTbCfYL4RW106Zg.png)

**Show**는 문자열로 표현 가능한 타입입니다. 대부분의 함수에서는 Show 타입클래스를 show로 취급하는데, show는 Show 계열의 어떤 값을 얻어서 문자열로 표시해줍니다.

![](https://cdn-images-1.medium.com/max/1600/1*WP0dpOsECs0-z1j5LlEHBA.png)

**Read**는 Show와 반대인 타입클래스라고 할 수 있습니다. read 함수는 문자열을 받아서 Read 계열의 타입으로 반환해줍니다.

![](https://cdn-images-1.medium.com/max/1600/1*DBEAjjPy_38h8phDf2IvQQ.png)

위와 같이 사용하면 예외가 발생하는 것을 볼 수 있는데, 이것은 무엇을 반환해야 되는지를 ghci가 추론할 수 없기 때문에 발생한 예외 입니다.

![](https://cdn-images-1.medium.com/max/1600/1*Hn6fRkmociKl14XNJXhL-Q.png)

read의 타입을 보면 Read 계열의 a를 리턴하는 것을 볼 수 있습니다. 따라서 **a의 타입을 컴파일러가 추론하지 못하면 예외가 발생**하게 됩니다. 이럴때 우리는 명시적으로 타입을 기술\(**type annotations\)**할 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*I1Q4fXXA8s6x-GPF0aVG0A.png)

타입 주석\(type annotation\)은 표현식의 타입 무엇인지를 명시적으로 알려주기위한 방법입니다. 위와 같이 표현식에 끝에 “::”를 사용하고 타입을 명시하면 됩니다.

![](https://cdn-images-1.medium.com/max/1600/1*ChtjnmyG8OkNkzxzzPSTzA.png)

Enum 계열은 순차적인 타입입니다. 따라서 차례대로 열거할 수 있습니다. Enum 계열인 경우는 리스트의 범위\(Range\)를 사용할 수 있습니다. 또한 succ, pred 함수의 파라메터로 넘길 수 있습니다. \(\), Bool, Char, Ordering, Int, Integer, Float, Double가 Enum 계열에 해당합니다.

![](https://cdn-images-1.medium.com/max/1600/1*dUutm22ttFnp6Aq8U6P9Rw.png)

Bounded는 최대값과 최소값을 가지는 타입입니다.

![](https://cdn-images-1.medium.com/max/1600/1*iCTz7sFz0W9TaddkDtNYzA.png)

minBound와 maxBound의 타입을 보면 다형성 상수라는 것을 확인하실 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*X_WcT0W4JSemaj_6EPCKiQ.png)

만약 튜플내의 컴포넌트들이 Bounded 계열이라면 튜플도 Bounded 계열이 됩니다.

![](https://cdn-images-1.medium.com/max/1600/1*DhbvkjFGE8S68SajBAOxxg.png)

**Num**은 숫자 타입클래스 입니다. 여기서 Num 계열은 다형성 상수인 것을 확인하실 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*LrNyShO9tee6KRZ9fM_WtQ.png)

위 예제에서 나온 타입은 모두 Num 계열입니다.

![](https://cdn-images-1.medium.com/max/1600/1*Ml-uDwJ8rYcyZtISUYcO_g.png)

\* 의 타입을 보면 \*가 모든 숫자들을 허용한다는 것을 확인하실 수 있습니다. \*는 두개의 동일한 타입의 숫자를 받아서 동일한 타입의 숫자를 반환합니다. 따라서 아래 예제와 같이 사용하면 두개의 타입이 다르기 때문에 에러가 나는 것을 확인할 수 있습니다.

![](https://cdn-images-1.medium.com/max/1600/1*39nSquctLsma7bm-pmx30w.png)

Num 계열의 타입이라면 이미 Show, Eq의 계열입니다.

**Integral**도 역시 숫자 타입클래스 입니다. Num 계열은 실수와 정수 모두를 포함하지만, Integral은 정수 전체만 포함합니다. Int와 Integer가 Integral 계열에 해당합니다.

**Floating**은 Float, Double와 같은 부동 소수점을 포함합니다.

![](https://cdn-images-1.medium.com/max/1600/1*Cux5alaciYlcNrCaAcic3A.png)

fromIntegral 함수는 숫자를 다루기위해서 매우 유용한 함수 입니다. 이 함수는 Integral을 받아서 좀 더 일반적인 Num을 반환해줍니다. \(여기서 여러개의 클래스 제약을 포함하는데, 이럴때는 괄호안에 ,로 구분하여 표시한다.\)이 기능은 정수와 부동소수점 타입이 함께 동작하게 할때 유용하게 사용됩니다.

![](https://cdn-images-1.medium.com/max/1600/1*AM5Q7eDxerT2yRtd7EU1oQ.png)

만약에 우리가 리스트의 길이를 얻어온 다음에 3.2를 리스트에 추가한다면 위와같이 타입이 다르기 때문에 에러가 발생할 것입니다.

![](https://cdn-images-1.medium.com/max/1600/1*21tapwkOpo_VSewcu2PhFA.png)

이때 위와 같이 fromIntegral를 사용하여 해결할 수 있습니다.



















