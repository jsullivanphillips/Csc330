require "./hw7.rb"

#Constants for testing
ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
EIGHT = 8.0
NINE = 9.0
TEN = 10.0

a = Point.new(ONE,TWO)
b = Line.new(THREE,FIVE)
c = VerticalLine.new(THREE)
d = LineSegment.new(ONE,TWO,-THREE,-FOUR)\

def equal(e1, e2)
    e1 == e2
end

d = Intersect.new(Line.new(ONE, TWO), Line.new(TWO, ZERO)).preprocess_prog.eval_prog([])
e = Point.new(TWO, FOUR)
puts(d.inspect)
puts(e.inspect)
puts(equal(Intersect.new(Line.new(ONE, TWO), Line.new(TWO, ZERO)).preprocess_prog.eval_prog([]), Point.new(TWO, FOUR)))

