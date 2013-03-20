description: Blocks should remove state after exiting 
value: ERROR

var x = 1;
{
    var y = 1;
}
return y;
