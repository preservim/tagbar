class
    POINT
inherit
    ANY
        redefine
            out
        end
create
    make, make_origin
 
feature -- Initialization
 
    make (a_x, a_y: INTEGER)
            -- Create with values `a_x' and `a_y'
        do
            set_x (a_x)
            set_y (a_y)
        ensure
            x_set: x = a_x
            y_set: y = a_y
        end
 
    make_origin
            -- Create at origin
        do
        ensure
            x_set: x = 0
            y_set: y = 0
        end
 
feature -- Access
 
    x: INTEGER assign set_x
            -- Horizontal axis coordinate
 
    y: INTEGER assign set_y
            -- Vertical axis coordinate
 
feature -- Element change
 
    set_x (a_x: INTEGER)
            -- Set `x' coordinate to `a_x'
        do
            x := a_x
        ensure
            x_set: x = a_x
        end
 
    set_y (a_y: INTEGER)
            -- Set `y' coordinate to `a_y'
        do
            y := a_y
        ensure
            y_set: y = a_y
        end
 
feature -- Output
 
    out: STRING
            -- Display as string
        do
            Result := "Point:   x = " + x.out + "   y = " + y.out
        end
end
