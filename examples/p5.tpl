start  := 1;
finish := 101;
step   := 2;
state  := (finish - state) * step;

if state > 0 then
(
  sum := 0;
  while start < finish do 
  (
    sum := sum + start;
    start := start + step;
  )
) 
else
(
  state  := -1;
  start  := -1;
  finish := -1;
  step   := -1;
)
