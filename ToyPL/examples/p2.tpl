times := 10 ;

x1 := 1 ;
x2 := 1 ;

ans := 0 ;

if times < 0 then
    ans := 0
else 
(
    if times = 1 or times = 2 then
        ans := 1
    else
    (
        i := 2;
        while i < times do
        (
            ans := x1 + x2;
            x1  := x2;
            x2  := ans;

            i   := i + 1
        )
    )
)
