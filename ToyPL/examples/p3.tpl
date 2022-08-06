max := 1000;
y   := 2;

ans := 0;

while y < max do
(
    x := 2;
    tmp := ans;

    while x < y do 
    (
        z := y / x;

        if z * x = y then
            tmp := y
        else
            tmp := tmp ;

        x := x + 1
    );
    
    if tmp = ans then
        ans := y
    else
        ans := ans ;

    y := y + 1
)
