http://nonconditional.com/2014/04/on-the-trick-for-computing-the-squared-euclidian-distances-between-two-sets-of-vectors/

    2 Channels (vec2 & vec4) = 6 column matrix
    +-----------+---------+-------------------+
    | channel   | 0       | 1                 |
    | component | x  | y  | r  | g  | b  | a  |
    |-----------+----+----+----+----+----+----+
    | element 0 |    |    |    |    |    |    |
    |         1 |    |    |    |    |    |    |
    |         2 |    |    |    |    |    |    |
    |         3 |    |    |    |    |    |    |
    |       ... |    |    |    |    |    |    |
    +-----------+---------+-------------------+

    Stored column wise in memory
    +---------------------------------------+-----------------------------
    | c0                                    | c1
    +----+----+----+----+----+----+----+----+----+----+----+----+----+----
    | 0  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 10 | 11 | 12 | ...
    | x0 | x1 | x2 | x3 | y0 | y1 | y2 | y3 | r0 | r1 | r2 | r3 | r4 | ...
