100 REM 高次代数方程式 DKA法 修正版
110 PRINT "DKA法による代数方程式の解法"
130 INPUT "次数(N) = "; N
140 DIM SA(N+1), SR(N+1), SI(N+1), SX(N+1), SY(N+1)
150 FOR I = 0 TO N
160   PRINT "係数 A("; N - I; ") = ";
170   INPUT SA(I)
180 NEXT I
190 IF SA(0) = 0 THEN PRINT "最高次係数が0です": STOP
205 REM --- 最高次係数で正規化 ---
210 FOR I = 1 TO N
220   SA(I) = SA(I) / SA(0)
230 NEXT I
240 SA(0) = 1
255 REM --- 初期値の設定 (円上に配置) ---
260 SW = 0
270 FOR I = 1 TO N
280   SQ = (ABS(SA(I))) ^ (1 / I)
290   IF SW < SQ THEN SW = SQ
300 NEXT I
310 IF SW = 0 THEN SW = 1
320 SP = 3.14159265353238
330 SB = 2 * SP / N
340 SC = SP / (2 * N)
350 FOR J = 1 TO N
360   ST = SB * (J - 1) + SC
370   SR(J) = SW * COS(ST)
380 SI(J) = SW * SIN(ST)
390 NEXT J
400 REM --- DKA法の反復計算ループ ---
410 M = 100: SE = 0.0000001
420 FOR K = 1 TO M
430   ERR = 0
440   FOR I = 1 TO N
        REM --- ホーナー法による多項式 P(z_i) の計算 ---
450     P1 = 1: P2 = 0
460     FOR J = 1 TO N
470       T1 = P1 * SR(I) - P2 * SI(I)
480       T2 = P1 * SI(I) + P2 * SR(I)
490       P1 = T1 + SA(J): P2 = T2
500     NEXT J
        REM --- 分母（他の根との差の積）の計算 ---
510     Q1 = 1: Q2 = 0
520     FOR J = 1 TO N
530       IF J = I THEN 580
540       DX = SR(I) - SR(J): DY = SI(I) - SI(J)
550       T1 = Q1 * DX - Q2 * DY
560       T2 = Q1 * DY + Q2 * DX
570       Q1 = T1: Q2 = T2
580     NEXT J
        REM --- 修正量の計算 Δz = P(z) / Q(z) ---
590     QW = Q1 * Q1 + Q2 * Q2
600     IF QW = 0 THEN QW = 1E-20
610     SX(I) = (P1 * Q1 + P2 * Q2) / QW
620     SY(I) = (P2 * Q1 - P1 * Q2) / QW
635   NEXT I
      REM --- 根の更新 ---
640   MAXDX = 0
650   FOR I = 1 TO N
660     SR(I) = SR(I) - SX(I)
670     SI(I) = SI(I) - SY(I)
680     IF ABS(SX(I)) > MAXDX THEN MAXDX = ABS(SX(I))
690     IF ABS(SY(I)) > MAXDX THEN MAXDX = ABS(SY(I))
700   NEXT I
      REM --- 収束判定 ---
710   IF MAXDX < SE THEN GOTO 760
720 NEXT K
730 PRINT "収束しませんでした (NON CONVERGENCE)"
740 GOTO 770
760 PRINT "--- 収束しました (回数="; K; ") ---"
775 REM --- 結果の表示 ---
780 PRINT "番号", "実数部", "虚数部"
790 FOR I = 1 TO N
800   PRINT I, SR(I), SI(I)
810 NEXT I
820 END