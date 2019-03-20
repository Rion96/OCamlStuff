LET N 3
LET X ARR N

FOR LET I 0; I < N; I++
    LET X[I] ARR N
ENDFOR

FOR LET I 0; I < N; I++
    FOR LET J 0; J < N; J++
        LET X[I][J] I + J
        PRINTLN X[I][J]
    ENDFOR
ENDFOR