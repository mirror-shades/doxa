struct Point {  
 x :: int, // unless marked, values are private
y :: int,

    pub id :: int,

    pub method New(x :: int, y :: int) { // methods must be marked pub as well
        return Point {
            x is x,
            y is y,
        }
    }

    method safeSub(a :: int, b :: int) returns int { // functions without the "this" parameter are static
        result is a - b
        if result > 255 or result < 0 then return -1
        return result
    }

    pub method get(this) { // when `this` is included, it becomes an instance method. this will be infered this as Point
        return Point {
            this.x,
            this.y
        }
    }

    pub method getDelta(this :: Point) returns int { // this can be typed explicitly if you want
        return Point.safeSub(this.x, this.y) // static methods can be called within instance methods
    }

}
