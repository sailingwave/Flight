FasdUAS 1.101.10   ��   ��    k             l     ����  r       	  m      
 
 �    L A X 	 o      ���� 
0 depart  ��  ��        l    ����  r        m       �    P E K  o      ���� 
0 arrive  ��  ��        l     ��������  ��  ��        l    ����  r        m    	   �    1 2 / 1 9 / 2 0 1 4  o      ���� 0 date_dep  ��  ��        l    ����  r         m     ! ! � " "  0 1 / 1 2 / 2 0 1 5   o      ���� 0 date_arr  ��  ��     # $ # l    % & ' % r     ( ) ( m    ����  ) o      ���� 0 flex_dep   & 1 +flexible date after the above dates (+days)    ' � * * V f l e x i b l e   d a t e   a f t e r   t h e   a b o v e   d a t e s   ( + d a y s ) $  + , + l    -���� - r     . / . m    ����   / o      ���� 0 flex_arr  ��  ��   ,  0 1 0 l     ��������  ��  ��   1  2 3 2 l    4���� 4 r     5 6 5 m     7 7 � 8 8 l / U s e r s / N a n / D r o p b o x / M a c h i n e L e a r n i n g / f l i g h t _ p r i c e _ f u n c . R 6 o      ���� 0 rsource  ��  ��   3  9 : 9 l    ;���� ; r     < = < m     > > � ? ? 8 / U s e r s / N a n / D e s k t o p / t e s t . h t m l = o      ���� 0 	html_file  ��  ��   :  @ A @ l    % B C D B r     % E F E m     ! G G � H H 6 / U s e r s / N a n / D e s k t o p / t e m p . c s v F o      ���� 0 outfile1   C  for all airlines    D � I I   f o r   a l l   a i r l i n e s A  J K J l  & - L M N L r   & - O P O m   & ) Q Q � R R 8 / U s e r s / N a n / D e s k t o p / t e m p 2 . c s v P o      ���� 0 outfile2   M  for AirChina    N � S S  f o r   A i r C h i n a K  T U T l     ��������  ��  ��   U  V W V l     �� X Y��   X ! convert date back to string    Y � Z Z 6 c o n v e r t   d a t e   b a c k   t o   s t r i n g W  [ \ [ i      ] ^ ] I      �� _���� 
0 tpdate   _  `�� ` o      ���� 
0 mydate  ��  ��   ^ k     F a a  b c b r      d e d l     f���� f n      g h g 1    ��
�� 
year h o     ���� 
0 mydate  ��  ��   e o      ���� 
0 myyear   c  i j i r     k l k c     m n m l   	 o���� o n    	 p q p m    	��
�� 
mnth q o    ���� 
0 mydate  ��  ��   n m   	 
��
�� 
long l o      ���� 0 mymonth   j  r s r Z     t u���� t A    v w v o    ���� 0 mymonth   w m    ���� 
 u r     x y x c     z { z b     | } | m     ~ ~ �    0 } o    ���� 0 mymonth   { m    ��
�� 
ctxt y o      ���� 0 mymonth  ��  ��   s  � � � r     ' � � � c     % � � � l    # ����� � n     # � � � 1   ! #��
�� 
day  � o     !���� 
0 mydate  ��  ��   � m   # $��
�� 
long � o      ���� 	0 myday   �  � � � Z   ( 9 � ����� � A  ( + � � � o   ( )���� 	0 myday   � m   ) *���� 
 � r   . 5 � � � c   . 3 � � � b   . 1 � � � m   . / � � � � �  0 � o   / 0���� 	0 myday   � m   1 2��
�� 
ctxt � o      ���� 	0 myday  ��  ��   �  � � � l  : :��������  ��  ��   �  ��� � L   : F � � b   : E � � � b   : C � � � b   : A � � � b   : ? � � � b   : = � � � m   : ; � � � � �   � o   ; <���� 
0 myyear   � m   = > � � � � �  - � o   ? @���� 0 mymonth   � m   A B � � � � �  - � o   C D���� 	0 myday  ��   \  � � � l     ��������  ��  ��   �  � � � l     ��������  ��  ��   �  � � � l     �� � ���   �  start    � � � � 
 s t a r t �  � � � l  . 5 ����� � r   . 5 � � � m   . 1 � � � � � 
 F A L S E � o      ���� 0 new_file  ��  ��   �  � � � l  6f ����� � Y   6f ��� � ��� � k   @a � �  � � � r   @ U � � � l  @ Q ����� � I   @ Q�� ����� 
0 tpdate   �  ��� � [   A M � � � l  A G ����� � 4   A G�� �
�� 
ldt  � o   E F���� 0 date_dep  ��  ��   � ]   G L � � � o   G H���� 0 day1   � 1   H K��
�� 
days��  ��  ��  ��   � o      ���� 0 dep   �  � � � l  V V��������  ��  ��   �  ��� � Y   Va ��� � ��� � k   `\ � �  � � � r   ` u � � � l  ` q ����� � I   ` q�� ����� 
0 tpdate   �  ��� � [   a m � � � l  a g ����� � 4   a g�� �
�� 
ldt  � o   e f���� 0 date_arr  ��  ��   � ]   g l � � � o   g h���� 0 day2   � 1   h k��
�� 
days��  ��  ��  ��   � o      ���� 0 arr   �  � � � e   v y � � o   v y���� 0 arr   �  � � � r   z � � � � b   z � � � � b   z � � � � b   z � � � � b   z � � � � b   z � � � � b   z � � � � b   z � � � � b   z  � � � m   z } � � � � � : h t t p : / / w w w . k a y a k . c o m / f l i g h t s / � o   } ~���� 
0 depart   � m    � � � � � �  - � o   � ����� 
0 arrive   � m   � � � � � � �  / � o   � ����� 0 dep   � m   � �   �  / � o   � ����� 0 arr   � m   � � �  / � o      �� 	0 myurl   �  l  � ��~�}�|�~  �}  �|    O   � �	 k   � �

  l  � ��{�{    activate    �  a c t i v a t e  r   � � o   � ��z�z 	0 myurl   n       1   � ��y
�y 
pURL 4   � ��x
�x 
docu m   � ��w�w   I  � ��v�u
�v .sysodelanull��� ��� nmbr m   � ��t�t �u    l  � ��s�r�q�s  �r  �q   �p r   � � n   � �  1   � ��o
�o 
conT  4   � ��n!
�n 
docu! m   � ��m�m  o      �l�l 0 mystring myString�p  	 m   � �""�                                                                                  sfri  alis    2  Mac                        � ˹H+   {�
Safari.app                                                      �z͜,r        ����  	                Applications    �<9      ͜��     {�  Mac:Applications: Safari.app   
 S a f a r i . a p p    M a c  Applications/Safari.app   / ��   #$# l  � ��k�j�i�k  �j  �i  $ %&% r   � �'(' 4   � ��h)
�h 
psxf) o   � ��g�g 0 	html_file  ( o      �f�f 0 newfile  & *+* I  � ��e,-
�e .rdwropenshor       file, o   � ��d�d 0 newfile  - �c.�b
�c 
perm. m   � ��a
�a boovtrue�b  + /0/ I  � ��`12
�` .rdwrwritnull���     ****1 o   � ��_�_ 0 mystring myString2 �^3�]
�^ 
refn3 o   � ��\�\ 0 newfile  �]  0 454 I  � ��[6�Z
�[ .rdwrclosnull���     ****6 o   � ��Y�Y 0 newfile  �Z  5 787 l  � ��X�W�V�X  �W  �V  8 9:9 l  � ��U�T�S�U  �T  �S  : ;<; l  � ��R=>�R  =  process in R   > �??  p r o c e s s   i n   R< @A@ r   �BCB b   �DED b   � �FGF m   � �HH �II  s o u r c e ( 'G o   � ��Q�Q 0 rsource  E m   � JJ �KK  ' )C o      �P�P 0 
cmd_source  A LML O  FNON k  EPP QRQ l �OST�O  S  activate   T �UU  a c t i v a t eR VWV I �NX�M
�N .DCMDDCMDnull���    utxtX o  �L�L 0 
cmd_source  �M  W Y�KY I E�JZ�I
�J .DCMDDCMDnull���    utxtZ b  A[\[ b  =]^] b  9_`_ b  5aba b  1cdc b  -efe b  )ghg b  %iji b  #klk b  mnm b  opo b  qrq m  ss �tt  f l i g h t _ s u m ( 'r o  �H�H 0 	html_file  p m  uu �vv  ' , 'n o  �G�G 
0 depart  l m  "ww �xx  ' , 'j o  #$�F�F 
0 arrive  h m  %(yy �zz  ' , a p p e n d =f o  ),�E�E 0 new_file  d m  -0{{ �||  , 'b o  14�D�D 0 outfile1  ` m  58}} �~~  ' , '^ o  9<�C�C 0 outfile2  \ m  =@ ���  ' )�I  �K  O m  	��|                                                                                  Rgui  alis      Mac                        � ˹H+   {�R.app                                                           %���h�        ����  	                Applications    �<9      �h�j     {�  Mac:Applications: R.app     R . a p p    M a c  Applications/R.app  / ��  M ��� l GG�B�A�@�B  �A  �@  � ��?� Z G\���>�=� = GN��� o  GJ�<�< 0 new_file  � m  JM�� ��� 
 F A L S E� r  QX��� m  QT�� ���  T R U E� o      �;�; 0 new_file  �>  �=  �?  �� 0 day2   � m   Y Z�:�:   � o   Z [�9�9 0 flex_arr  ��  ��  �� 0 day1   � m   9 :�8�8   � o   : ;�7�7 0 flex_dep  ��  ��  ��   � ��� l     �6�5�4�6  �5  �4  � ��� l     �3�2�1�3  �2  �1  � ��� l g���0�/� O  g���� k  m��� ��� l mm�.���.  �  activate   � ���  a c t i v a t e� ��� I m|�-��,
�- .DCMDDCMDnull���    utxt� b  mx��� b  mt��� m  mp�� ��� ( a i r _ s u m   =   r e a d . c s v ( '� o  ps�+�+ 0 outfile1  � m  tw�� ���  ' )�,  � ��� I }��*��)
�* .DCMDDCMDnull���    utxt� m  }��� ��� P a i r _ s u m   =   a i r _ s u m [ o r d e r ( a i r _ s u m $ p r i c e ) , ]�)  � ��(� I ���'��&
�' .DCMDDCMDnull���    utxt� b  ����� b  ����� m  ���� ��� & w r i t e . c s v ( a i r _ s u m , '� o  ���%�% 0 outfile1  � m  ���� ��� , ' , r o w . n a m e s = F , q u o t e = F )�&  �(  � m  gj��|                                                                                  Rgui  alis      Mac                        � ˹H+   {�R.app                                                           %���h�        ����  	                Applications    �<9      �h�j     {�  Mac:Applications: R.app     R . a p p    M a c  Applications/R.app  / ��  �0  �/  � ��$� l     �#�"�!�#  �"  �!  �$       "� ��� 
   !�� 7 > G Q���������������������   �  �������
�	��������� ������������������������������� 
0 tpdate  
� .aevtoappnull  �   � ****� 
0 depart  � 
0 arrive  � 0 date_dep  � 0 date_arr  �
 0 flex_dep  �	 0 flex_arr  � 0 rsource  � 0 	html_file  � 0 outfile1  � 0 outfile2  � 0 new_file  � 0 dep  � 0 arr  � 	0 myurl  �  0 mystring myString�� 0 newfile  �� 0 
cmd_source  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  ��  � �� ^���������� 
0 tpdate  �� ����� �  ���� 
0 mydate  ��  � ���������� 
0 mydate  �� 
0 myyear  �� 0 mymonth  �� 	0 myday  � �������� ~���� � � � �
�� 
year
�� 
mnth
�� 
long�� 

�� 
ctxt
�� 
day �� G��,E�O��,�&E�O�� �%�&E�Y hO��,�&E�O�� �%�&E�Y hO�%�%�%�%�%� �����������
�� .aevtoappnull  �   � ****� k    ���  ��  ��  ��  ��  #��  +��  2��  9��  @��  J��  ���  ��� �����  ��  ��  � ������ 0 day1  �� 0 day2  � A 
�� �� �� !�������� 7�� >�� G�� Q�� ������������� � � � ��"��������������������������HJ�����suwy{}��������� 
0 depart  �� 
0 arrive  �� 0 date_dep  �� 0 date_arr  �� �� 0 flex_dep  �� 0 flex_arr  �� 0 rsource  �� 0 	html_file  �� 0 outfile1  �� 0 outfile2  �� 0 new_file  
�� 
ldt 
�� 
days�� 
0 tpdate  �� 0 dep  �� 0 arr  �� 	0 myurl  
�� 
docu
�� 
pURL�� 
�� .sysodelanull��� ��� nmbr
�� 
conT�� 0 mystring myString
�� 
psxf�� 0 newfile  
�� 
perm
�� .rdwropenshor       file
�� 
refn
�� .rdwrwritnull���     ****
�� .rdwrclosnull���     ****�� 0 
cmd_source  
�� .DCMDDCMDnull���    utxt����E�O�E�O�E�O�E�O�E�OjE�O�E�O�E�O�E` Oa E` Oa E` O/j�kh  **a �/�_  k+ E` O
j�kh **a �/�_  k+ E` O_ Oa �%a %�%a %_ %a %_ %a %E` Oa   '_ *a !k/a ",FOa #j $O*a !k/a %,E` &UO*a '�/E` (O_ (a )el *O_ &a +_ (l ,O_ (j -Oa .�%a /%E` 0Oa 1 ;_ 0j 2Oa 3�%a 4%�%a 5%�%a 6%_ %a 7%_ %a 8%_ %a 9%j 2UO_ a :  a ;E` Y h[OY��[OY��Oa 1 )a <_ %a =%j 2Oa >j 2Oa ?_ %a @%j 2U� �  � ���  2 0 1 4 - 1 2 - 2 4� ���  2 0 1 5 - 0 1 - 1 2� ��� v h t t p : / / w w w . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 /� ���88 88 < ! D O C T Y P E   h t m l > 
 < ! - - [ i f   l t   I E   7 ] >   < h t m l   c l a s s = " n o - j s   l t - i e 9   l t - i e 8   l t - i e 7   _ g w w p d f o q   _ s k 8 j g y n a   _ e a a d b x k s   _ x f s e 4 y a 9   _ t a h o r n 7 f   _ r 1 r w s z x f   "   >   < ! [ e n d i f ] - - > 
 < ! - - [ i f   I E   7 ] >   < h t m l   c l a s s = " n o - j s   l t - i e 9   l t - i e 8   _ g w w p d f o q   _ s k 8 j g y n a   _ e a a d b x k s   _ x f s e 4 y a 9   _ t a h o r n 7 f   _ r 1 r w s z x f   "   >   < ! [ e n d i f ] - - > 
 < ! - - [ i f   I E   8 ] >   < h t m l   c l a s s = " n o - j s   l t - i e 9   _ g w w p d f o q   _ s k 8 j g y n a   _ e a a d b x k s   _ x f s e 4 y a 9   _ t a h o r n 7 f   _ r 1 r w s z x f   "   >   < ! [ e n d i f ] - - > 
 < ! - - [ i f   g t   I E   8 ] > < ! - - >   < h t m l   c l a s s = " n o - j s   _ g w w p d f o q   _ s k 8 j g y n a   _ e a a d b x k s   _ x f s e 4 y a 9   _ t a h o r n 7 f   _ r 1 r w s z x f   "   >   < ! - - < ! [ e n d i f ] - - > 
 < h e a d   p r e f i x = " o g :   h t t p : / / o g p . m e / n s #   f b :   h t t p : / / o g p . m e / n s / f b #   k a y a k d o t c o m :   h t t p : / / o g p . m e / n s / f b / k a y a k d o t c o m # " > 
 < m e t a   h t t p - e q u i v = " c o n t e n t - l a n g u a g e "   c o n t e n t = " e n - u s " > < m e t a   n a m e = " k a y a k _ p a g e "   c o n t e n t = " f l i g h t , r e s u l t s , u n k n o w n " > 
 < m e t a   n a m e = " g o o g l e - s i g n i n - c l i e n t i d "   c o n t e n t = " 4 4 6 0 0 9 5 2 5 3 4 4 - j 4 2 0 p 2 1 u 4 a p 0 1 1 4 q r 1 f n r k 8 b h q 4 f 2 i l 1 . a p p s . g o o g l e u s e r c o n t e n t . c o m "   / > < m e t a   n a m e = " g o o g l e - s i g n i n - c o o k i e p o l i c y "   c o n t e n t = " s i n g l e _ h o s t _ o r i g i n "   / > < m e t a   n a m e = " g o o g l e - s i g n i n - c a l l b a c k "   c o n t e n t = " g o o g l e A u t h C a l l b a c k "   / > < m e t a   n a m e = " g o o g l e - s i g n i n - s c o p e "   c o n t e n t = " h t t p s : / / w w w . g o o g l e a p i s . c o m / a u t h / p l u s . l o g i n   h t t p s : / / w w w . g o o g l e a p i s . c o m / a u t h / u s e r i n f o . e m a i l "   / > 
 < m e t a   h t t p - e q u i v = " C o n t e n t - T y p e "   c o n t e n t = " t e x t / h t m l ;   c h a r s e t = u t f - 8 "   / > 
 < m e t a   n a m e = " r o b o t s "   c o n t e n t = " n o i n d e x , n o f o l l o w , n o o d p "   / > 
 < m e t a   p r o p e r t y = " f b : a p p _ i d "   c o n t e n t = " 1 6 3 0 0 3 0 7 9 2 0 4 " / > 
 < m e t a   p r o p e r t y = " o g : t i t l e "   c o n t e n t = " L o s   A n g e l e s & n b s p ; t o & n b s p ; B e i j i n g & n b s p ;   W e d   D e c   2 4   2 0 1 4   & n d a s h ;   M o n   J a n   1 2   2 0 1 5 "   / > 
 < m e t a   p r o p e r t y = " o g : t y p e "   c o n t e n t = " k a y a k d o t c o m : f l i g h t _ s e a r c h "   / > 
 < m e t a   p r o p e r t y = " o g : u r l "   c o n t e n t = " h t t p : / / w w w . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 "   / > 
 < m e t a   p r o p e r t y = " o g : i m a g e "   c o n t e n t = " h t t p : / / c d n 4 . k a y a k . c o m / r e s / i m a g e s / f a c e b o o k / o g - s h a r e - f l i g h t . p n g ? v = 8 a 9 0 9 2 a 7 b 6 a a 2 7 e c 5 e 1 2 e f d f 4 5 0 2 5 1 e 2 0 0 f d d a b 0 "   / > 
 < l i n k   r e l = " a p p l e - t o u c h - i c o n "   h r e f = " h t t p : / / c d n 5 . k a y a k . c o m / r e s / i m a g e s / l o g o s / k a y a k _ k _ i O S _ h o m e _ s c r e e n _ 5 7 x 5 7 . p n g ? v = d b b 7 a 9 d 0 b 3 8 f a 3 0 e a c a 5 f b d 2 3 e 2 e b 5 7 8 f f b 4 e 1 d e "   / > 
 < l i n k   r e l = " a p p l e - t o u c h - i c o n "   s i z e s = " 7 2 x 7 2 "   h r e f = " h t t p : / / c d n 1 . k a y a k . c o m / r e s / i m a g e s / l o g o s / k a y a k _ k _ i O S _ h o m e _ s c r e e n _ 7 2 x 7 2 . p n g ? v = 2 9 2 e 4 8 b 5 6 9 1 3 0 5 4 f f f 8 1 5 2 9 3 8 2 1 8 5 d c 8 2 d 9 0 6 5 3 1 "   / > 
 < l i n k   r e l = " a p p l e - t o u c h - i c o n "   s i z e s = " 1 1 4 x 1 1 4 "   h r e f = " h t t p : / / c d n 4 . k a y a k . c o m / r e s / i m a g e s / l o g o s / k a y a k _ k _ i O S _ h o m e _ s c r e e n _ 1 1 4 x 1 1 4 . p n g ? v = f 4 1 1 7 6 4 e 9 2 b c 5 5 e b 1 c 3 2 8 5 7 d a a 8 a 8 5 d a 8 5 9 2 8 c 8 1 "   / > 
 < m e t a   n a m e = " t w i t t e r : c a r d "   c o n t e n t = " a p p " > 
 < m e t a   n a m e = " t w i t t e r : a p p : i d : i p h o n e "   c o n t e n t = " i d 3 0 5 2 0 4 5 3 5 " > 
 < m e t a   n a m e = " t w i t t e r : a p p : i d : i p a d "   c o n t e n t = " i d 3 0 5 2 0 4 5 3 5 " > 
 < m e t a   n a m e = " t w i t t e r : a p p : i d : g o o g l e p l a y "   c o n t e n t = " c o m . k a y a k . a n d r o i d " > 
 < l i n k   r e l = " s h o r t c u t   i c o n "   h r e f = " / k . i c o "   / > 
 < m e t a   n a m e = " m s a p p l i c a t i o n - T i l e I m a g e "   c o n t e n t = " h t t p : / / c d n 2 . k a y a k . c o m / r e s / i m a g e s / k - l o g o - 1 4 4 . p n g ? v = 6 c c 6 b 1 e 5 0 e 4 2 b e 6 f d 2 d 2 f 3 a 1 c 5 1 f 2 c 3 b 9 c 4 d a 0 8 f "   / > 
 < m e t a   n a m e = " m s a p p l i c a t i o n - T i l e C o l o r "   c o n t e n t = " # f 2 8 5 2 0 "   / > 
 < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / w i d g e t s / r 9 w i d g e t s . c s s ? s p a r k l e & v = 6 e 3 a b 1 4 9 e 9 3 4 9 7 e 4 1 1 5 a 4 c a 7 1 8 9 5 9 9 c 5 7 d c 9 9 c 1 7 "   t y p e = " t e x t / c s s " / > 
 < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / a l i e n / r 9 a l i e n . c s s ? v = 9 a b 8 8 2 5 5 a 3 f 9 5 2 d f a 8 8 0 1 3 a f 3 c 2 7 c 1 3 b 2 1 3 a c 2 2 c "   t y p e = " t e x t / c s s " / > 
 < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / r 9 . c s s ? v = 9 d c 0 8 f 3 6 d d 6 7 e 8 4 c 0 e 0 a 3 b 0 b 7 0 5 8 e 3 3 6 b c 0 3 a 9 9 e "   t y p e = " t e x t / c s s " / > 
 < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / f l i g h t / r 9 . f l i g h t . c s s ? v = 8 9 a d 2 6 1 5 7 8 5 d f 9 0 9 7 9 4 d 2 1 f 0 2 f 6 1 6 7 0 9 f c b 9 1 9 5 f "   t y p e = " t e x t / c s s " / > 
 < ! - - [ i f   l t   I E   9 ] >   < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / r 9 . i e 8 . c s s ? v = b 5 8 a 3 8 6 1 e 0 a 9 8 8 2 f e 3 9 d c e 1 4 5 8 7 6 a c 3 b f a 0 7 3 3 4 0 "   t y p e = " t e x t / c s s " / > 
 < ! [ e n d i f ] - - > 
 < ! - - [ i f   l t   I E   8 ] >   < l i n k   r e l = " s t y l e s h e e t "   h r e f = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / c s s / r 9 . i e 7 . c s s ? v = 2 5 8 b 0 6 1 e e 2 e a c 7 0 7 e 0 3 e 7 f 0 6 3 2 d 4 3 7 4 a 6 a 1 9 1 e e d "   t y p e = " t e x t / c s s " / > 
 < ! [ e n d i f ] - - > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   c h a r s e t = " u t f - 8 " > 
 / * 
 * *   L o a d   G o o g l e   j a v a s c r i p t   a d s e n s e   i m p l e m e n t a t i o n 
 *   T h i s   n e e d s   t o   g o   i n   t h e   H E A D . 
 * / 
 ( f u n c t i o n ( G , o , O , g , L , e ) { G [ g ] = G [ g ] | | f u n c t i o n ( ) { ( G [ g ] [ ' q ' ] = G [ g ] [ ' q ' ] | | [ ] ) . p u s h ( 
 a r g u m e n t s ) } , G [ g ] [ ' t ' ] = 1 * n e w   D a t e ; L = o . c r e a t e E l e m e n t ( O ) , e = o . g e t E l e m e n t s B y T a g N a m e ( 
 O ) [ 0 ] ; L . a s y n c = 1 ; L . s r c = ' / / w w w . g o o g l e . c o m / a d s e n s e / s e a r c h / a s y n c - a d s . j s ' ; 
 e . p a r e n t N o d e . i n s e r t B e f o r e ( L , e ) } ) ( w i n d o w , d o c u m e n t , ' s c r i p t ' , ' _ g o o g C s a ' ) ;   < / s c r i p t > 
 < t i t l e > K A Y A K   S e a r c h   R e s u l t s < / t i t l e > 
 < / h e a d > 
 < b o d y   c l a s s = " " > 
 < ! - - [ i f   l t   I E   7 ] > 
 < p   c l a s s = " c h r o m e f r a m e " > Y o u   a r e   u s i n g   a n   < s t r o n g > o u t d a t e d < / s t r o n g >   b r o w s e r .   P l e a s e   < a   h r e f = " h t t p : / / b r o w s e h a p p y . c o m / " > u p g r a d e   y o u r   b r o w s e r < / a >   o r   < a   h r e f = " h t t p : / / w w w . g o o g l e . c o m / c h r o m e f r a m e / ? r e d i r e c t = t r u e " > a c t i v a t e   G o o g l e   C h r o m e   F r a m e < / a >   t o   i m p r o v e   y o u r   e x p e r i e n c e . < / p > 
 < ! [ e n d i f ] - - > 
 < d i v   i d = " h d "   c l a s s = " h d   r e s u l t s H e a d e r   
 " > 
 < d i v   c l a s s = " l i n e r " > 
 < d i v   i t e m s c o p e   i t e m t y p e = " h t t p : / / s c h e m a . o r g / O r g a n i z a t i o n " > < a   h r e f = " / "   i d = " l o g o "   i t e m p r o p = " u r l " > < / a > < / d i v > 
 < d i v   i d = " h e a d e r m a i n t a b s " > 
 < a   i d = " h o t e l s - l i n k "   c l a s s = " "   h r e f = " / h o t e l s " > H O T E L S < / a > 
 < a   i d = " f l i g h t s - l i n k "   c l a s s = " n a v s e l e c t e d "   h r e f = " / f l i g h t s " > F L I G H T S < / a > 
 < a   i d = " c a r s - l i n k "   c l a s s = " "   h r e f = " / c a r s " > C A R S < / a > 
 < a   i d = " k p a c k - l i n k "   c l a s s = " "   h r e f = " / p a c k a g e s " > P A C K A G E S < / a > 
 < a   i d = " d e a l s - l i n k "   c l a s s = " "   o n c l i c k = " r e t u r n   s e t u p S e a r c h P a r a m s ( ' ' ) ; "   h r e f = " / d e a l s " > D E A L S < / a > 
 < a   i d = " m o r e - l i n k "   h r e f = " / m o r e " > 
 M O R E 
 < / a > 
 < / d i v > 
 < d i v   i d = " h e a d e r " > 
 < s p a n   c l a s s = " h e a d e r I t e m "   i d = ' h e a d e r M y T r i p s L i n k ' > 
 < a   h r e f = " / t r i p s " > M Y   T R I P S < / a > 
 < / s p a n > 
 < s p a n   c l a s s = " h e a d e r I t e m "   i d = ' m y A c c o u n t H e a d e r L i n k ' > 
 < d i v   c l a s s = " h e a d e r - t o o l t i p - b a s e "   d a t a - o f f s e t = " 5 0 " > 
 < a   c l a s s = " b r e a d c r u m b s "   h r e f = " # "   i d = " h e a d e r s i g n i n l i n k "   o n c l i c k = " i f ( w i n d o w . e v e n t ) { i f ( e v e n t . p r e v e n t D e f a u l t ) { e v e n t . p r e v e n t D e f a u l t ( ) ; } e v e n t . r e t u r n V a l u e   =   f a l s e ; } A j a x R e g . r e g t y p e = ' t o p r i g h t s i g n i n ' ; A j a x R e g . u i . l o g i n ( ) ; "   r e l = " n o f o l l o w " > L O G I N < / a > 
 & n b s p ; 
 < / d i v > 
 < / s p a n > 
 < s p a n   c l a s s = " h e a d e r I t e m " > 
 < s p a n   i d = " c u r r e n c y P i c k e r l i n k "   c l a s s = " c u r r e n c y P i c k e r l i n k " > 
 < s p a n > $ < / s p a n > 
 < d i v   i d = " c u r r e n c y L i s t " > 
 < d i v   i d = " c u r r e n c y _ t o o l t i p "   c l a s s = " h e a d e r - t o o l t i p " > 
 < d i v   c l a s s = " t i p " > < / d i v > 
 < d i v   c l a s s = " o u t e r " > 
 < d i v   c l a s s = " i n n e r " > 
 < d i v   c l a s s = " i t e m s " > 
 < d i v   c l a s s = " h e a d e r " > T o p   C u r r e n c i e s < / d i v > 
 < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   a c t i v e "   d a t a - c u r = " U S D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > U n i t e d   S t a t e s   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " E U R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > � < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > E u r o < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C A D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > C a n a d i a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " G B P " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > � < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > U K   P o u n d s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " A U D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > A u s t r a l i a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < d i v   c l a s s = " h e a d e r " > A l l   C u r r e n c i e s < / d i v > 
 < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " A R S " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > A r g e n t i n i a n   P e s o s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " A U D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > A u s t r a l i a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " B D T " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > T k < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > B a n g l a d e s h   T a k a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " B R L " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > R $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > B r a z i l a n   R e a i s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C A D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > C a n a d i a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C L P " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > C h i l e a n   P e s o s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C N Y " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 2 0 8 0 3 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > C h i n e s e   Y u a n   R e n m i n b i < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C Z K " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > K & # 2 6 9 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > C z e c h   K o r u n a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " D K K " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > k r < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > D a n i s h   K r o n e r < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " E G P " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & p o u n d ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > E g y p t i a n   P o u n d s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " E U R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & e u r o ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > E u r o < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " F J D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > F i j i a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " H K D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > H K $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > H o n g   K o n g   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " H U F " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > F t < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > H u n g a r i a n   F o r i n t < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " I S K " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > k r < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > I c e l a n d i c   K r o n a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " I N R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 8 3 7 7 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > I n d i a   R u p e e s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " I D R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > R p < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > I n d o n e s i a n   R u p i a h < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " I L S " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 8 3 6 2 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > I s r a e l i   N e w   S h e k e l s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " J M D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > J a m a i c a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " J P Y " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & y e n ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > J a p a n e s e   Y e n < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " K W D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 1 5 8 3 ; . & # 1 6 0 3 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > K u w a i t i   D i n a r < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " L T L " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > L t < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > L i t h u a n i a n   L i t a s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " M Y R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > M $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > M a l a y s i a n   R i n g g i t < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " M X N " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > M e x i c a n   P e s o s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " B T C " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 3 6 4 7 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > m i l l i b i t c o i n < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " T W D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > N T $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > N e w   T a i w a n   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " N Z D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > N e w   Z e a l a n d   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " N O K " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > k r < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > N o r w e g i a n   K r o n e r < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " P K R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 8 3 6 0 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > P a k i s t a n i   R u p e e s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " P H P " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 8 3 6 9 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > P h i l i p p i n e s   P e s o s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " P L N " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > z & # 3 2 2 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > P o l i s h   Z l o t y < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " Q A R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 6 5 0 2 0 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > Q a t a r i   R i y a l s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " R O N " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > l e i < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > R o m a n i a n   N e w   L e u < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " R U B " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 1 0 8 8 ; . < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > R u s s i a n   R u b l e s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " S A R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 6 5 0 2 0 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S a u d i   A r a b i a n   R i y a l s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " S G D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > S $ < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S i n g a p o r e   D o l l a r s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " Z A R " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > R < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S o u t h   A f r i c a n   R a n d < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " K R W " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 8 3 6 1 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S o u t h   K o r e a n   W o n < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " S E K " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > k r < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S w e d i s h   K r o n o r < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " C H F " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > S F r . < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > S w i s s   F r a n c < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " T H B " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 3 6 4 7 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > T h a i   B a h t < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " T R Y " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > T L < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > T u r k i s h   L i r a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " A E D " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 1 5 8 3 ; . & # 1 5 7 3 ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > U A E   D i r h a m s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " G B P " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & p o u n d ; < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > U K   P o u n d s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   "   d a t a - c u r = " U A H " > 
 < d i v   c l a s s = " f l o a t L e f t   s y m b o l " > & # 1 0 7 5 ; & # 1 0 8 8 ; & # 1 0 8 5 ; . < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c u r r e n c y n a m e " > U k r a i n i a n   H r y v n i a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / s p a n > 
 < / s p a n > 
 < s p a n   c l a s s = " h e a d e r I t e m " > 
 < d i v   c l a s s = " c o u n t r y P i c k e r " > 
 < d i v   i d = " c o u n t r y p i c k e r l i n k " > 
 < s p a n   c l a s s = " f l a g   g r a y s c a l e   u s " > < / s p a n > 
 < d i v   i d = " c o u n t r y L i s t " > 
 < d i v   i d = " c o u n t r y _ t o o l t i p "   c l a s s = " h e a d e r - t o o l t i p "   d a t a - h r e f = " h t t p : / / w w w . k a y a k . c o m / " > 
 < d i v   c l a s s = " t i p " > < / d i v > 
 < d i v   c l a s s = " o u t e r " > 
 < d i v   c l a s s = " i n n e r " > 
 < d i v   c l a s s = " i t e m s " > 
 < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . a r . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e s "   h r e f l a n g = " e s - A R "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   a r " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   a r " > A r g e n t i n a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m . a u / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - A U "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   a u " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   a u " > A u s t r a l i a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . b e . k a y a k . c o m / " 
 l a n g = " f r "   h r e f l a n g = " f r - B E "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   b e " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   b e " > B e l g i q u e < / d i v > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " f r "   h r e f l a n g = " f r - B E " 
 d a t a - h r e f = " h t t p : / / w w w . b e . k a y a k . c o m / i n ? u r l = / & c c = b e & a = k a y a k & p = & l c = f r "   > F R < / s p a n > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " n l "   h r e f l a n g = " n l - B E " 
 d a t a - h r e f = " h t t p : / / w w w . b e . k a y a k . c o m / i n ? u r l = / & c c = b e & a = k a y a k & p = & l c = n l "   > N L < / s p a n > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m . b r / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " p t "   h r e f l a n g = " p t - B R "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   b r " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   b r " > B r a s i l < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . c a . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - C A "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   c a " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   c a " > C a n a d a < / d i v > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " e n "   h r e f l a n g = " e n - C A " 
 d a t a - h r e f = " h t t p : / / w w w . c a . k a y a k . c o m / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = c a & a = k a y a k & p = & l c = e n "   > E N < / s p a n > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " f r "   h r e f l a n g = " f r - C A " 
 d a t a - h r e f = " h t t p : / / w w w . c a . k a y a k . c o m / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = c a & a = k a y a k & p = & l c = f r "   > F R < / s p a n > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . c n . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " z h "   h r e f l a n g = " z h - C N "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   c n " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   c n " >N-V� < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . d k / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " d a "   h r e f l a n g = " d a - D K "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   d k " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   d k " > D a n m a r k < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . d e / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " d e "   h r e f l a n g = " d e - D E "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   d e " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   d e " > D e u t s c h l a n d < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . e s / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e s "   h r e f l a n g = " e s - E S "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   e s " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   e s " > E s p a � a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . f r / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " f r "   h r e f l a n g = " f r - F R "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   f r " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   f r " > F r a n c e < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . g r . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e l "   h r e f l a n g = " e l - G R "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   g r " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   g r " >������ < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m . h k / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - H K "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   h k " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   h k " > H o n g   K o n g < / d i v > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " e n "   h r e f l a n g = " e n - H K " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . c o m . h k / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = h k & a = k a y a k & p = & l c = e n "   > E N < / s p a n > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " z h "   h r e f l a n g = " z h - H K " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . c o m . h k / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = h k & a = k a y a k & p = & l c = z h "   >N-e� < / s p a n > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o . i n / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - I N "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   i n " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   i n " > I n d i a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . i e / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - I E "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   i e " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   i e " > I r e l a n d < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . i t / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " i t "   h r e f l a n g = " i t - I T "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   i t " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   i t " > I t a l i a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . j p . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " j a "   h r e f l a n g = " j a - J P "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   j p " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   j p " >e�g, < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m . m x / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e s "   h r e f l a n g = " e s - M X "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   m x " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   m x " > M � x i c o < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . n l / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " n l "   h r e f l a n g = " n l - N L "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   n l " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   n l " > N e d e r l a n d < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . n z . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - N Z "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   n z " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   n z " > N e w   Z e a l a n d < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . n o / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " n o "   h r e f l a n g = " n o - N O "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   n o " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   n o " > N o r g e < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . p l / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " p l "   h r e f l a n g = " p l - P L "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   p l " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   p l " > P o l s k a < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . p t / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " p t "   h r e f l a n g = " p t - P T "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   p t " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   p t " > P o r t u g a l < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > < d i v   c l a s s = " c o l u m n " > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . r u / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " r u "   h r e f l a n g = " r u - R U "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   r u " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   r u " > >AA8O < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c h / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " d e "   h r e f l a n g = " d e - C H "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   c h " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   c h " > S c h w e i z < / d i v > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " f r "   h r e f l a n g = " f r - C H " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . c h / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = c h & a = k a y a k & p = & l c = f r "   > F R < / s p a n > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " d e "   h r e f l a n g = " d e - C H " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . c h / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = c h & a = k a y a k & p = & l c = d e "   > D E < / s p a n > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . s g / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - S G "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   s g " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   s g " > S i n g a p o r e < / d i v > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " e n "   h r e f l a n g = " e n - S G " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . s g / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = s g & a = k a y a k & p = & l c = e n "   > E N < / s p a n > 
 < s p a n   c l a s s = " f l o a t L e f t   l a n g u a g e L i n k   "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) "   l a n g = " z h "   h r e f l a n g = " z h - S G " 
 d a t a - h r e f = " h t t p : / / w w w . k a y a k . s g / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = s g & a = k a y a k & p = & l c = z h "   >N-e� < / s p a n > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . f i . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " f i "   h r e f l a n g = " f i - F I "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   f i " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   f i " > S u o m i < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . s e / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " s v "   h r e f l a n g = " s v - S E "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   s e " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   s e " > S v e r i g e < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . t w . k a y a k . c o m / " 
 l a n g = " z h "   h r e f l a n g = " z h - T W "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   t w " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   t w " >S�pc < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m . t r / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " t r "   h r e f l a n g = " t r - T R "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   t r " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   t r " > T � r k i y e < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   " 
 h r e f = " h t t p : / / w w w . k a y a k . c o . u k / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - G B "   > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   g b " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   g b " > U n i t e d   K i n g d o m < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < a   c l a s s = " i t e m   a c t i v e " 
 h r e f = " h t t p : / / w w w . k a y a k . c o m / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " 
 l a n g = " e n "   h r e f l a n g = " e n - U S "   d a t a - h r e f = " h t t p : / / w w w . k a y a k . c o m / i n ? u r l = / f l i g h t s / L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / & c c = u s & a = k a y a k & p = & l c = " > 
 < d i v   c l a s s = " f l o a t L e f t   f l a g   u s " > < / d i v > 
 < d i v   c l a s s = " f l o a t L e f t   c o u n t r y n a m e   u s " > U n i t e d   S t a t e s < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / a > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r f i x " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / s p a n > 
 < / d i v > 
 < d i v   i d = " p o p u p T T I P P o s "   s t y l e = " v i s i b i l i t y :   h i d d e n ;   p o s i t i o n : a b s o l u t e ;   l e f t :   9 0 p x ;   t o p :   1 p x ; " > < / d i v > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " s u b - h d "   c l a s s = " s u b - h d   r e s u l t s S u b H e a d e r   f l i g h t R e s u l t s S u b H e a d e r   
 " > 
 < / d i v > 
 < d i v   i d = " b d "   r o l e = " m a i n "   c l a s s = " b d   v - f l i g h t   p - r e s u l t s   r e s u l t s C o n t e n t   
 " > 
 < s t y l e   t y p e = " t e x t / c s s " > 
 . g o b u t t o n c e l l   { 
 w i d t h :   4 0 p x ; 
 p a d d i n g - r i g h t :   5 p x   ! i m p o r t a n t ; 
 } 
 . d e t a i l s i t e   { w i d t h :   2 7 0 p x ;   w h i t e - s p a c e :   n o w r a p ;   } 
 . h o t e l t o t a l   { m i n - w i d t h :   1 5 5 p x ;   } 
 . f l i g h t t o t a l ,   . c a r t o t a l   { p a d d i n g - r i g h t   :   1 0 p x ; } 
 < / s t y l e > 
 < s p a n   i t e m s c o p e   i t e m t y p e = " h t t p : / / s c h e m a . o r g / W e b P a g e / " > 
 < s p a n   i t e m p r o p = " o p e r a t i o n "   i t e m s c o p e   i t e m t y p e = " h t t p : / / s c h e m a . o r g / V i e w A c t i o n " > 
 < s p a n   i t e m p r o p = " a c t i o n H a n d l e r "   i t e m s c o p e   i t e m t y p e = " h t t p : / / s c h e m a . o r g / W i n d o w s A c t i o n H a n d l e r " > 
 < m e t a   i t e m p r o p = " a p p l i c a t i o n I d "   c o n t e n t = " 1 1 5 b 1 3 d e - 2 d 8 e - 4 c 7 0 - 9 a 3 6 - d f d 2 c 6 a 7 a 9 2 3 " > 
 < m e t a   i t e m p r o p = " p a c k a g e F a m i l y N a m e "   c o n t e n t = " K A Y A K . c o m . K A Y A K T r a v e l _ 7 w g 9 e w 7 y d e j 3 j " > 
 < m e t a   i t e m p r o p = " m i n V e r s i o n "   c o n t e n t = " 2 . 2 . 1 . 0 " > 
 < m e t a   i t e m p r o p = " a r g u m e n t s "   c o n t e n t = " v e r t i c a l = h o t e l & p a g e = c i t y _ h o t e l s & c i d = 3 2 8 6 " > 
 < / s p a n > 
 < s p a n   i t e m p r o p = " a c t i o n H a n d l e r "   i t e m s c o p e   i t e m t y p e = " h t t p : / / s c h e m a . o r g / W i n d o w s P h o n e A c t i o n H a n d l e r " > 
 < m e t a   i t e m p r o p = " a p p l i c a t i o n I d "   c o n t e n t = " b 2 6 c 5 a a e - d e a 7 - e 0 1 1 - 9 8 6 b - 7 8 e 7 d 1 f a 7 6 f 8 " > 
 < m e t a   i t e m p r o p = " m i n V e r s i o n "   c o n t e n t = " 2 . 0 . 9 . 0 " > 
 < m e t a   i t e m p r o p = " a r g u m e n t s "   c o n t e n t = " v e r t i c a l = h o t e l & p a g e = c i t y _ h o t e l s & c i d = 3 2 8 6 " > 
 < / s p a n > 
 < / s p a n > 
 < / s p a n > 
 < d i v   i d = " w 1 1 r p "   c l a s s = " y u i - t 4   f i x e d W i d t h O u t e r   f l i g h t O u t e r R e s u l t s   l i s t V i e w O u t e r   " > 
 < d i v   i d = " r e s u l t s P a n e "   r o l e = " m a i n "   c l a s s = " b d   f a d e a b l e " > 
 < d i v   i d = " y u i - m a i n " > 
 < d i v   i d = " m a i n b l o c k "   c l a s s = " y u i - b " > 
 < d i v   c l a s s = " y u i - b " > 
 < d i v   c l a s s = " y u i - g f " > 
 < d i v   c l a s s = " i n l i n e S e a r c h A g a i n   i n l i n e T w o C o l S e a r c h A g a i n "   i d = " i n l i n e s e a r c h b l o c k " > 
 < f o r m   i d = " i n l i n e s e a r c h a g a i n "   n a m e = " i n l i n e s e a r c h a g a i n "   m e t h o d = " P O S T "   a c t i o n = " / f l i g h t s "   c l a s s = " i n l i n e F r o m W i t h P o w e r F l e x " > 
 < d i v   > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " a c t i o n "   i d = " a c t i o n "   v a l u e = " d o f l i g h t s "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " o r i g i n s i d "   v a l u e = " J l A C B _ r Q C 4 "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " o n e w a y "   v a l u e = " n "   / > 
 < i n p u t   t y p e = " h i d d e n "   i d = " i n l i n e d e s t c o d e "   n a m e = " d e s t c o d e "   v a l u e = " P E K / 3 2 8 6 "   / > 
 < i n p u t   t y p e = " h i d d e n "   i d = " i n l i n e o r i g i n c o d e "   n a m e = " o r i g i n c o d e "   v a l u e = " L A X / 1 6 0 7 8 "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " d t F l e x C a t "   v a l u e = " e x a c t "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " d e p a r t _ d a t e _ f l e x "   v a l u e = " e x a c t "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " r e t u r n _ d a t e _ f l e x "   v a l u e = " e x a c t "   / > 
 < i n p u t   n a m e = " d e p a r t _ t i m e "   t y p e = " h i d d e n "   v a l u e = " a "   / > 
 < i n p u t   n a m e = " r e t u r n _ t i m e "   t y p e = " h i d d e n "   v a l u e = " a "   / > 
 < i n p u t   t y p e = " h i d d e n "   v a l u e = " 1 "   n a m e = " a d u l t s "   / > 
 < i n p u t   t y p e = " h i d d e n "   v a l u e = " 0 "   n a m e = " s e n i o r s "   / > 
 < i n p u t   t y p e = " h i d d e n "   v a l u e = " 0 "   n a m e = " c h i l d r e n "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " c a b i n "   v a l u e = " e "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " d a t e l i m i t "   v a l u e = " "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " t a b "   v a l u e = " f l i g h t s "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " t r a c k f i l t e r s "   v a l u e = " t r u e "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " s r c "   v a l u e = " m o d i f y f l i g h t s e a r c h "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " u s e c l e a n u r l "   v a l u e = " t r u e "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " u s e m o b i l e u r l "   v a l u e = " f a l s e "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " u s e l a s t "   v a l u e = " f a l s e "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " p a g e _ o r i g i n "   v a l u e = " "   / > 
 < d i v   c l a s s = " f i e l d I n p u t   f i e l d I n p u t L o c a t i o n " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " o r i g i n "   i d = " i n l i n e o r i g i n "   s i z e = " 1 7 "   v a l u e = " L o s   A n g e l e s   ( L A X ) "   c l a s s = " i n i t i a l F o r m F i e l d   r 9 - s m a r t y - i n p u t   s e l e c t T e x t O n F o c u s "   p l a c e h o l d e r = " F r o m " / > 
 < / d i v > 
 < d i v   c l a s s = " f i e l d I n p u t   f i e l d I n p u t L o c a t i o n " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " d e s t i n a t i o n "   i d = " i n l i n e d e s t i n a t i o n "   v a l u e = ' B e i j i n g   ( P E K ) '   c l a s s = " m i d d l e F o r m F i e l d   r 9 - s m a r t y - i n p u t   s e l e c t T e x t O n F o c u s "   p l a c e h o l d e r = " T o " / > 
 < / d i v > 
 < d i v   c l a s s = " f i e l d I n p u t   f i e l d I n p u t T r a v e l D a t e s " > 
 < d i v   i d = " t r a v e l _ d a t e s "   c l a s s = " r 9 - d a t e p i c k e r - w r a p p e r   r 9 - d a t e p i c k e r - s t a r t - e m p t y   r 9 - d a t e p i c k e r - e n d - e m p t y   d a t e P i c k e r   m i d d l e F o r m F i e l d " > < s p a n   c l a s s = " r 9 - d a t e p i c k e r - i c o n " > < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - s t a r t "   c l a s s = " r 9 - d a t e p i c k e r - s e c t i o n   r 9 - d a t e p i c k e r - s t a r t " > < s p a n   i d = " t r a v e l _ d a t e s - s t a r t - d i s p l a y "   c l a s s = " r 9 - d a t e p i c k e r - d i s p l a y " > < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - s t a r t - p l a c e h o l d e r "   c l a s s = " r 9 - d a t e p i c k e r - p l a c e h o l d e r " > D e p a r t < / s p a n > < / s p a n > < s p a n   c l a s s = " r 9 - d a t e p i c k e r - s e p a r a t o r " > & n d a s h ; < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - e n d "   c l a s s = " r 9 - d a t e p i c k e r - s e c t i o n   r 9 - d a t e p i c k e r - e n d " > < s p a n   i d = " t r a v e l _ d a t e s - e n d - d i s p l a y "   c l a s s = " r 9 - d a t e p i c k e r - d i s p l a y " > < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - e n d - p l a c e h o l d e r "   c l a s s = " r 9 - d a t e p i c k e r - p l a c e h o l d e r " > R e t u r n < / s p a n > < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - s u m a r y "   c l a s s = " r 9 - d a t e p i c k e r - s e c t i o n   r 9 - d a t e p i c k e r - s u m m a r y " > < s p a n   i d = " t r a v e l _ d a t e s - s u m m a r y - d i s p l a y "   c l a s s = " r 9 - d a t e p i c k e r - s u m m a r y - d i s p l a y " > < / s p a n > < / s p a n > < s p a n   i d = " t r a v e l _ d a t e s - c l e a r "   c l a s s = " r 9 - d a t e p i c k e r - c l e a r " > < / s p a n > < i n p u t   t y p e = " t e x t "   i d = " t r a v e l _ d a t e s - t a b "   c l a s s = " r 9 - d a t e p i c k e r - t a b "   r e a d o n l y = " t r u e " > < i n p u t   t y p e = " t e x t "   i d = " i n l i n e _ d e p a r t _ d a t e "   n a m e = " d e p a r t _ d a t e "   v a l u e = " 1 2 / 2 4 / 2 0 1 4 "   c l a s s = " r 9 - d a t e p i c k e r - i n p u t   r 9 - d a t e p i c k e r - s t a r t " > < i n p u t   t y p e = " t e x t "   i d = " i n l i n e _ r e t u r n _ d a t e "   n a m e = " r e t u r n _ d a t e "   v a l u e = " 0 1 / 1 2 / 2 0 1 5 "   c l a s s = " r 9 - d a t e p i c k e r - i n p u t   r 9 - d a t e p i c k e r - e n d " > < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " f i e l d I n p u t   f i e l d I n p u t B u t t o n " > 
 < b u t t o n   c l a s s = " u i - b u t t o n   f i n a l F o r m F i e l d   i n l i n e - b u t t o n   f l i g h t s   "   t y p e = " s u b m i t " > < s p a n > S e a r c h < / s p a n > < / b u t t o n >   < / d i v > 
 < d i v   t i t l e = " M o d i f y   a l l   s e a r c h   o p t i o n s "   i d = " b a c k T o F r o n t D o o r S e a r c h A g a i n "   c l a s s = " f u l l S e a r c h A g a i n B l o c k " > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   i d = " i n l i n e S e a r c h T o F D B u t t o n "   t y p e = " b u t t o n " > < s p a n > < i m g   s r c = ' h t t p : / / c d n 4 . k a y a k . c o m / r e s / i m a g e s / i c o n s / a - 1 4 x 1 4 - 6 6 6 - p l u s . p n g ? v = 6 6 8 3 2 1 2 b 1 a a b 6 6 8 a 9 2 e 0 4 0 a d 0 b 2 3 1 1 0 6 0 4 0 e d c e 8 '   / > < / s p a n > < / b u t t o n >   < / d i v > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / f o r m > 
 < d i v   c l a s s = " t h e L i n c o l n L i n e " > < / d i v > 
 < / d i v > 
 < d i v   i d = " f i l t e r b l o c k "   c l a s s = " y u i - u   f i r s t " > 
 < d i v   i d = " n r L e f t F i l t e r "   c l a s s = " n r L e f t F i l t e r   s q u a r e t o p "   >   
 < d i v   c l a s s = " f i l t e r S e c t i o n C o n t e n t "   i d = " f s _ c o n t e n t _ t o o l b o x " > 
 < d i v   c l a s s = " t o o l b o x A c t i o n s " > 
 < d i v > 
 < d i v   i d = " s h o w a l e r t l i n k r o w " > 
 < s p a n   c l a s s = " b u l l e t " > & b u l l ;   < / s p a n > < a   c l a s s = " a c t i o n l i n k "   i d = " s h o w a l e r t l i n k "   t i t l e = " C r e a t e   a   p r i c e   a l e r t " 
 h r e f = " j a v a s c r i p t : s h o w A l e r t A j a x R e g ( - 1 ,   - 1 ,   ' ' , ' t o o l b o x ' ) " > P r i c e   a l e r t < / a > 
 < / d i v > 
 < d i v   i d = " s h o w a l e r t s a v e d l i n k r o w "   s t y l e = " d i s p l a y : n o n e ; " > 
 < s p a n   c l a s s = " g r e e n " > < b > S a v e d ! < / b > < / s p a n > 
 < a   c l a s s = " a c t i o n l i n k "   t a r g e t = " _ b l a n k "   i d = " e d i t a l e r t l i n k "   h r e f = " / a l e r t s ? a c t i o n = p r e p o p & a l e r t i d = " > e d i t < / a > & n b s p ; 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " b a g g a g e F e e L i n k " > & b u l l ;   < a   t a r g e t = " a i r l i n e f e e s "   h r e f = ' / a i r l i n e - f e e s ' > A i r l i n e   f e e s < / a > < / d i v > 
 < d i v   c l a s s = " t o o l b o x C l e a r " > < / d i v > 
 < d i v   c l a s s = " e x p l o r e F l e x D a t e O p t i o n s " > 
 F l e x   D a t e s :   < a   i d = " p l u s M i n u s T h r e e M a t r i x L i n k "   h r e f = " j a v a s c r i p t : v o i d ( 0 ) ; "   o n c l i c k = " t o g g l e P l u s M i n u s T h r e e ( ) " > & p l u s m n ; 3   d a y s < / a > 
 < s p a n   c l a s s = " s e p a r a t o r " > | < / s p a n > 
 < a   o n c l i c k = " r e t u r n   R 9 . R P . F l i g h t s . P o w e r F l e x . R o u n d T r i p . t o g g l e ( t h i s ) "   t i t l e = " S h o w   f l e x i b l e   d a t e   c h a r t s " > 
 E x p l o r e r   < / a > 
 < / d i v > 
 < d i v   c l a s s = " t o o l b o x C l e a r " > < / d i v > 
 < / d i v > 
 < d i v   i d = " r a i l f l y i n f o "   s t y l e = " d i s p l a y : n o n e ; " > 
 R a i l   &   F l y   i s   a v a i l a b l e   o n   v a r i o u s   a i r l i n e s   i n   c o o p e r a t i o n   w i t h   D e u t s c h e   B a h n .   A t   a   l o w   f i x e d   p r i c e   t r a v e l e r s   o n   t h e   s h o r t e s t   p a t h   f r o m   a n y   s t a t i o n   t o   i t s   a i r p o r t   a n d   b a c k .   < a   h r e f = " "   t a r g e t = " _ b l a n k " > F u r t h e r   d e t a i l s   a v a i l a b l e   o n   p r o v i d e r   w e b s i t e < / a > 
 < / d i v > 
 < d i v   c l a s s = " t o o l b o x C l e a r " > < / d i v > 
 < d i v   i d = " p r o g r e s s W r a p p e r " > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < d i v   i d = " p r o g r e s s D i v "   s t y l e = " h e i g h t :   1 0 p x ;   m a r g i n - b o t t o m :   6 p x " > < / d i v > 
 < d i v   s t y l e = " p a d d i n g :   0 ; "   i d = " f l i p p y i f r a m e c o n t a i n e r " > 
 < t a b l e   c e l l s p a c i n g = " 0 "   c e l l p a d d i n g = " 0 "   c l a s s = " s m a l l c o r n e r s   f l i p p y b o x "   > < t r   c l a s s = " t o p r o w " > < t d   c l a s s = " b o r d e r c e l l " > < i m g   c l a s s = " c o r n e r i m a g e "   b o r d e r = " 0 "   s r c = " h t t p : / / c d n 1 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / f f f - n - c c c - t l . g i f ? v = 0 8 c 7 8 a 8 9 0 e 2 7 f e 5 4 7 2 1 1 2 a e 5 5 b f f c 7 a 9 9 6 d c 7 5 7 f " > < / t d > < t d   c l a s s = " b o r d e r c e l l "   s t y l e = " b a c k g r o u n d - i m a g e : u r l ( h t t p : / / c d n 4 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / n - c c c - t . g i f ? v = 9 0 7 f f 9 e e e f 8 1 5 4 b c 1 9 3 3 6 c 4 b 4 b 0 2 3 8 b 5 c e 3 3 2 9 e f ) ;   b a c k g r o u n d - r e p e a t :   r e p e a t - x ; " > < / t d > < t d   c l a s s = " b o r d e r c e l l " > < i m g   c l a s s = " c o r n e r i m a g e "   s r c = " h t t p : / / c d n 5 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / f f f - n - c c c - t r . g i f ? v = 7 0 b f 8 9 5 9 8 3 5 a d e 5 b 3 a e 0 0 9 5 7 1 2 0 f 5 f f 9 3 d 5 2 5 5 1 5 " > < / t d > < / t r > < t r > < t d   c l a s s = " r e p e a t y "   s t y l e = " b a c k g r o u n d - i m a g e : u r l ( h t t p : / / c d n 2 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / n - c c c - l . g i f ? v = d 4 3 e 3 b 5 b 2 2 8 3 1 b 6 3 8 d 4 9 1 5 7 8 b 8 4 2 e 9 c f d a 3 6 f f f b ) " > < / t d > < t d   s t y l e = " v e r t i c a l - a l i g n : m i d d l e ; p a d d i n g :   0 ; w i d t h : 1 0 0 % ; " > 
 < i m g   s r c = ' h t t p : / / c d n 2 . k a y a k . c o m / r e s / i m a g e s / f l i p p y / a - f l i p p y - f l i g h t - a n i . g i f ? v = 0 2 a 0 7 5 2 8 0 5 e 6 8 5 c c b d 0 1 0 a 6 b 2 2 0 4 e 3 5 7 c 6 1 7 1 3 6 4 '   w i d t h = " 1 6 9 " / > 
 < / t d > < t d   c l a s s = " r e p e a t y "   s t y l e = " b a c k g r o u n d - i m a g e : u r l ( h t t p : / / c d n 1 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / n - c c c - r . g i f ? v = e 5 b 8 f b 4 5 5 5 5 a f 1 1 a 3 2 d 2 a e 4 9 7 a c 4 1 4 1 1 7 e 3 0 3 7 2 0 ) " > < / t d > < / t r > < t r   c l a s s = " b o t t o m r o w " > < t d   c l a s s = " b o r d e r c e l l " > < i m g   c l a s s = " c o r n e r i m a g e "   s r c = " h t t p : / / c d n 4 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / f f f - n - c c c - b l . g i f ? v = b 7 d e 0 7 d 8 4 7 3 5 7 b e d b c b 2 8 8 5 b 9 3 0 9 1 f e 2 d b d 0 2 6 e 9 " > < / t d > < t d   c l a s s = " b o r d e r c e l l "   s t y l e = " b a c k g r o u n d - i m a g e : u r l ( h t t p : / / c d n 2 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / n - c c c - b . g i f ? v = e e 2 6 2 1 b 2 5 b b 1 7 c e a c d 5 5 f 1 4 1 0 b 5 a 1 c 4 5 c d 7 b 4 8 5 8 ) ;   b a c k g r o u n d - r e p e a t :   r e p e a t - x ; " > < / t d > < t d   c l a s s = " b o r d e r c e l l " > < i m g   c l a s s = " c o r n e r i m a g e "   s r c = " h t t p : / / c d n 3 . k a y a k . c o m / r e s / i m a g e s / c o r n e r s / n e w / f f f - n - c c c - b r . g i f ? v = 1 1 4 7 0 4 e 9 3 6 c 2 6 b b 9 2 1 5 6 0 0 4 0 d 1 5 9 7 c c a b 8 0 4 a a d c " > < / t d > < / t r > < / t a b l e >   < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " s e c t i o n C l e a r " > < / d i v > 
 < / d i v > 
 < d i v   i d = " t o p O f F i l t e r " > < / d i v > 
 < d i v   i d = " l e f t R e s u l t L i s t " > < / d i v > 
 < f o r m   n a m e = " r e s u l t U I "   i d = " r e s u l t U I "   a c t i o n = " j a v a s c r i p t : r e f i l t e r ( ) ; " > < i n p u t   t y p e = " h i d d e n "   n a m e = " t o p r o w "   v a l u e = " 0 "   / > < i n p u t   t y p e = " h i d d e n "   n a m e = " m i n p r i c e "   v a l u e = " - 1 "   / > < i n p u t   t y p e = " h i d d e n "   n a m e = " m a x p r i c e "   v a l u e = " - 1 "   / > < i n p u t   t y p e = " h i d d e n "   n a m e = " l a s t s o r t i d "   v a l u e = " "   / > < i n p u t   t y p e = " h i d d e n "   n a m e = " l a s t f i l t e r e d l i s t "   i d = " l a s t f i l t e r e d l i s t "   v a l u e = " "   / > < i n p u t   t y p e = " h i d d e n "   n a m e = " l a s t p a g e n u m "   v a l u e = " "   / > < d i v   i d = " f i l t e r d i v "   c l a s s = " " >   
 < / d i v > < / f o r m > 
 < / d i v >   
 < b r   c l a s s = " h i d e f o r m a p " / > 
 < d i v   i d = " n r L e f t r a i l A d "   c l a s s = ' h i d e f o r m a p ' > 
 < s p a n   i d = " d i s p l a y A d S p a n F r a m e 4 " > 
 < d i v   b o r d e r = " 0 "   m a r g i n w i d t h = " 0 "   m a r g i n h e i g h t = " 0 "   s t y l e = " m a r g i n - t o p : 0 p x ; o v e r f l o w : h i d d e n ; "   c l a s s = " n o n d s   G P T "   i d = " d i s p l a y A d 4 " > < / d i v > 
 < / s p a n > 
 < d i v   i d = " d i s p l a y A d H i d e 4 "   c l a s s = " h i d e d i s p l a y a d "   s t y l e = " v i s i b i l i t y :   h i d d e n ; " > 
 < a   c l a s s = " h i d e d i s p l a y a d   a c t i o n l i n k "   h r e f = " j a v a s c r i p t :   h i d e D i s p l a y A d s ( t r u e ) " > 
 < s p a n   i d = " h i d e d i s p m s g 2 " > h i d e   d i s p l a y   a d s < / s p a n > 
 < / a > 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = ' c l e a r ' > < / d i v > 
 < / d i v > < d i v   i d = " r e s b o d y "   c l a s s = " y u i - u " > 
 < ! - -   S p i n n e r   f o r   p r o g r e s s   - - > 
 < d i v   c l a s s = " r e s b o d y S p i n n e r C o n t a i n e r "   s t y l e = " d i s p l a y :   n o n e ; " > 
 < d i v   c l a s s = " r 9 - s p i n n e r   r e s b o d y S p i n n e r " > < / d i v > 
 < d i v   c l a s s = " R P T o p T e x t " > 
 S e a r c h i n g . . .   < / d i v > 
 < / d i v >   < d i v   i d = " r i g h t - w h i t e - b a c k g r o u n d " > < / d i v >   
 < d i v   i d = " l i s t b o d y "   > 
 < d i v   i d = " o u t e r P i n n e d I t e m s C o n t a i n e r "   c l a s s = " f l i g h t l i s t " > < / d i v > 
 < d i v   i d = " r e s u l t s L i s t H e a d e r "   c l a s s = " r e s u l t s L i s t H e a d e r " > 
 < d i v   c l a s s = " r e s u l t s H e a d e r R o w " > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " f i l t e r m a t r i x c o n t a i n e r "   c l a s s = " f i l t e r m a t r i x c o n t a i n e r " > 
 < d i v   c l a s s = " n r M a t r i x   h i d d e n   l a z y   "   i d = " f i l t e r v i e w m a t r i x " > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = ' f l e x d a t e s s e c t i o n '   c l a s s = " l a z y "   > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < d i v   i d = " t o p O f R e s u l t s M e s s a g e H o l d e r " > 
 < / d i v > 
 < d i v   c l a s s = " i n l i n e A d C o n t a i n e r   " > 
 < d i v   i d = " t o p A d C o n t a i n e r "   c l a s s = " i n n e r   i n l i n e A d C o n t e n t   h i d e e m p t y f i r s t " > 
 < d i v   i d = " a b o v e R e s u l t s T e x t A d s "   c l a s s = " w i d e A d T a b l e   w i d e A d O n T o p " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = ' c o n t e n t _ d i v '   c l a s s = " r e s u l t s W r a p p e r S e c t i o n   f l i g h t l i s t   i n P r o g r e s s " > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " b o t t o m P a g i n g A n d A d s " > 
 < d i v   i d = " b o t t o m P a g e C o n t r o l s "   c l a s s = " b o t t o m P a g e C o n t r o l s " > 
 < / d i v > 
 < d i v   i d = " b o t t o m A d C o n t a i n e r " >   < t a b l e   i d = " b o t t o m a d s _ t a b l e "   b o r d e r = " 0 "   c e l l s p a c i n g = " 0 "   c e l l p a d d i n g = " 0 "   s t y l e = " w i d t h :   1 0 0 % " > 
 < d i v   s t y l e = " h e i g h t :   1 2 p x " > < / d i v > 
 < t r   i d = " b o t t o m a d s _ a d s _ t r " > 
 < t d   s t y l e = " w i d t h :   5 0 % ;   m a r g i n - r i g h t :   4 p x "   i d = " b o t t o m a d s _ t e x t _ a d s _ t d " > < / t d > 
 < t d   i d = " b o t t o m a d s _ d i s p l a y _ a d _ t d "   a l i g n = " r i g h t "   v a l i g n = " t o p " > 
 < t a b l e   b o r d e r = " 0 "   c e l l s p a c i n g = " 0 "   c e l l p a d d i n g = " 0 "   s t y l e = " w i d t h :   1 0 0 % ;   m a r g i n - t o p :   6 p x " > 
 < t r > 
 < t d   i d = " b a n n e r a d 3 "   s t y l e = " w i d t h :   1 0 0 % ; " > 
 < s p a n   i d = " d i s p l a y A d S p a n F r a m e 3 " > 
 < d i v   b o r d e r = " 0 "   m a r g i n w i d t h = " 0 "   m a r g i n h e i g h t = " 0 "   s t y l e = " m a r g i n - t o p : 0 p x ; o v e r f l o w : h i d d e n ; "   c l a s s = " n o n d s   G P T "   i d = " d i s p l a y A d 3 " > < / d i v > 
 < / s p a n > 
 < d i v   i d = " d i s p l a y A d H i d e 3 "   c l a s s = " h i d e d i s p l a y a d "   s t y l e = " v i s i b i l i t y :   h i d d e n ; " > 
 < a   c l a s s = " h i d e d i s p l a y a d   a c t i o n l i n k "   h r e f = " j a v a s c r i p t :   h i d e D i s p l a y A d s ( t r u e ) " > 
 < s p a n   i d = " h i d e d i s p m s g 2 " > h i d e   d i s p l a y   a d s < / s p a n > 
 < / a > 
 < / d i v > 
 < / t d > 
 < / t r > 
 < / t a b l e > 
 < / t d > 
 < / t r > 
 < / t a b l e > 
 < / d i v >   < / d i v >   < d i v   i d = " b o t t o m L e g a l C o n t a i n e r " > 
 < d i v   i d = " t o p d i s c l a i m e r s p a c e r "   c l a s s = " l o n g h o t e l d i s c l a i m e r " > < / d i v > 
 < t a b l e   c l a s s = " d i s c l a i m e r s T a b l e "   s t y l e = " w i d t h :   1 0 0 % " > 
 < t r > 
 < t d > 
 < t a b l e > 
 < t r > 
 < t d   c l a s s = " d o t T e x t "   v a l i g n = " t o p " > * < / t d > 
 < t d   c l a s s = " d i s c l a i m e r T e x t " > 
 P r i c e s   a r e   p e r   p e r s o n   a n d   a r e   f o r   e & n d a s h ; t i c k e t s   a n d   i n c l u d e   a l l   t a x e s   & a m p ;   f e e s   i n   U S D . < b r > < b r > W e   m a k e   e v e r y   a t t e m p t   t o   g e t   a c c u r a t e   p r i c e s ,   h o w e v e r ,   < a   h r e f = " j a v a s c r i p t :   s h o w F u l l D i s c l a i m e r T e x t S e c t i o n ( ) ; " > p r i c e s   a r e   n o t   g u a r a n t e e d < / a > .   < d i v   c l a s s = " f u l l P r i c e G u a r a n t e e D i s c l a i m e r " > < d i v   c l a s s = " a r t i c l e - s e c t i o n   f i r s t - s e c t i o n " > 
 < p > H e r e & # 3 9 ; s   w h y : < / p > 
 < p > 
 < b > W e  r e   n o t   t h e   s e l l e r < / b > 
 < b r   / > 
 A t   K A Y A K   w e   d o n  t   s e t   p r i c e s ,   s o   i t  s   n o t   p o s s i b l e   f o r   u s   t o   g u a r a n t e e   w h a t   o t h e r   c o m p a n i e s   a r e   s e l l i n g . 
 < / p > 
 < p > 
 < b > W e   a g g r e g a t e   t o n s   o f   d a t a   f o r   y o u < / b > 
 < b r   / > 
 O u r   s e r v i c e   l e t s   y o u   q u i c k l y   a n d   e a s i l y   c o m p a r e   r e s u l t s   f r o m   h u n d r e d s   o f   t r a v e l   s i t e s   a t   o n c e .   I n   t h i s   s e n s e ,   K A Y A K   i s   a   s e a r c h   e n g i n e   a n d   w e   d o n & # 3 9 ; t   g u a r a n t e e   p r i c e s   w e   f i n d . 
 < / p > 
 < p > 
 < b > W h y   a r e n  t   p r i c e s   a c c u r a t e   1 0 0 %   o f   t h e   t i m e ? < / b > 
 < b r   / > 
 P r i c e s   o n   a i r l i n e   s e a t s ,   h o t e l   r o o m s   a n d   c a r   r e n t a l s   c a n   c h a n g e   f r e q u e n t l y .   S e v e r a l   p e o p l e   m a y   a l s o   b e   t r y i n g   t o   b u y   t h e   s a m e   t r a v e l   o p t i o n   s i m u l t a n e o u s l y .   A s   a   r e s u l t ,   y o u   m a y   f i n d   o n   o c c a s i o n   t h a t   c e r t a i n   p r i c e s   a r e   n o   l o n g e r   a v a i l a b l e . 
 < / p > 
 < p > 
 < b > M o r e   q u e s t i o n s   o r   i f   y o u   h a v e   a   b a d   p r i c e   t o   r e p o r t   t o   u s < / b > 
 < b r   / > 
 P l e a s e   s e n d   u s   a n 
 < a   h r e f = " / k / f e e d b a c k / f o r m " > e m a i l < / a > 
 a n d   s o m e o n e   f r o m   o u r   t e a m   w i l l   g e t   b a c k   t o   y o u   p r o m p t l y . 
 < / p > 
 < / d i v > < / d i v > 
 < / t d > 
 < / t r > 
 < t r   i d = " h a c k e r F a r e D i s c l a i m e r "   s t y l e = " d i s p l a y : n o n e " > 
 < t d   v a l i g n = " t o p " > < s p a n   c l a s s = " h a c k e r S y m b o l " > & s u p 1 ; < / s p a n > 
 < / t d > 
 < t d   c l a s s = " d i s c l a i m e r T e x t " > H a c k e r   F a r e s   s e l l   t i c k e t s   t o / f r o m   a   d e s t i n a t i o n   v i a   d i f f e r e n t   a i r l i n e s   a n d   a r e   s u b j e c t   t o   t h e   b o o k i n g 
 r e q u i r e m e n t s   a n d   t e r m s   o f   e a c h .   A n y   c h a n g e s   m a d e   t o   o n e   o f   y o u r   t i c k e t s   w i l l   n o t   n e c e s s a r i l y   a f f o r d 
 r i g h t s   t o   c h a n g e   t h e   o t h e r   t i c k e t .   F a r e s   c h a n g e   f r e q u e n t l y   a n d   a r e   s u b j e c t   t o   a v a i l a b i l i t y . 
 I n t e r n a t i o n a l   t r a v e l   m a y   r e q u i r e   p r o o f   o f   r e t u r n   f l i g h t . < / t d > 
 < / t r > 
 < / t a b l e > 
 < / t d > 
 < / t r > 
 < / t a b l e > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " r i g h t a d s "   c l a s s = " y u i - b " > 
 < d i v   i d = " r i g h t a d s i n n e r " > 
 < d i v   i d = " t r a c e o u t p u t "   s t y l e = " d i s p l a y : n o n e " > < / d i v > 
 < d i v   i d = " c m p 2 c o n t a i n e r "   s t y l e = " d i s p l a y : n o n e ; " > < / d i v > 
 < d i v   i d = " n r A d s "   c l a s s = " n r A d s " > 
 < d i v   c l a s s = " b a n n e r A d "   i d = " b a n n e r a d 1 " > < c e n t e r >   < s p a n   i d = " d i s p l a y A d S p a n F r a m e 1 " > 
 < d i v   b o r d e r = " 0 "   m a r g i n w i d t h = " 0 "   m a r g i n h e i g h t = " 0 "   s t y l e = " m a r g i n - t o p : 0 p x ; o v e r f l o w : h i d d e n ; "   c l a s s = " n o n d s   G P T "   i d = " d i s p l a y A d 1 " > < / d i v > 
 < / s p a n > 
 < d i v   i d = " d i s p l a y A d H i d e 1 "   c l a s s = " h i d e d i s p l a y a d "   s t y l e = " v i s i b i l i t y :   h i d d e n ; "   > 
 < a   c l a s s = " h i d e d i s p l a y a d   a c t i o n l i n k "   h r e f = " j a v a s c r i p t :   h i d e D i s p l a y A d s ( t r u e ) " > 
 < s p a n   i d = " h i d e d i s p m s g " > h i d e   d i s p l a y   a d s < / s p a n > 
 < / a > 
 < / d i v > 
 < / c e n t e r > < / d i v > 
 < d i v   i d = " c m p 2 t o p r i g h t "   c l a s s = " c m p 2 R i g h t R a i l " > 
 < d i v   > 
 < h 2 > C o m p a r e   S i t e s   v s .   K A Y A K < / h 2 > 
 < d i v   i d = " c m p 2 t o p r i g h t r a i l c o n t a i n e r "   c l a s s = " c m p 2 r i g h t r a i l c o n t a i n e r " > 
 < d i v > 
 < d i v   c l a s s = " p r o v i d e r s e c t i o n " > 
 < d i v   o n c l i c k = " w i n d o w . o p e n ( ' / s / c l i c k t h r o u g h . j s p ? p l i d = 8 2 5 0 9 1 3 & c p n i d = 5 0 0 6 3 2 7 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 & p r v = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = v 8 1 O Z v k d y i U l U W U Y B - r x G - o 8 4 I Y & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 1 ' ) ; "   c l a s s = " p r o v i d e r i t e m " > 
 < i m g   s r c = " / v 7 0 1 / k i m g / c o m p a r e 2 / l o g o s / 8 9 a d f a a 6 a 3 e d a c 6 b . p n g "   / > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   o n c l i c k = " j q . n o o p ( ) ; "   t y p e = " b u t t o n " > < s p a n > C o m p a r e   & r a q u o ; < / s p a n > < / b u t t o n > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " p r o v i d e r s e c t i o n " > 
 < d i v   o n c l i c k = " w i n d o w . o p e n ( ' / s / c l i c k t h r o u g h . j s p ? p l i d = 5 1 6 1 2 3 9 & c p n i d = 7 0 0 9 2 3 9 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = P R I C E L I N E _ F L O A T _ U S _ C M P 2 & p r v = P R I C E L I N E _ F L O A T _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = P R I C E L I N E _ F L O A T _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = Z g H r N Z w k e A W N z A 2 f M t 0 y F f Y r B D E & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 2 ' ) ; "   c l a s s = " p r o v i d e r i t e m " > 
 < i m g   s r c = " / v 7 0 1 / k i m g / l o g o s / 8 5 1 8 3 e 0 3 3 2 9 8 1 e 7 a . p n g "   / > 
 < i m g   s t y l e = " b o r d e r : n o n e "   s r c = " "   w i d t h = " 1 "   h e i g h t = " 1 " / > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   o n c l i c k = " j q . n o o p ( ) ; "   t y p e = " b u t t o n " > < s p a n > C o m p a r e   & r a q u o ; < / s p a n > < / b u t t o n > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " p r o v i d e r s e c t i o n " > 
 < d i v   o n c l i c k = " w i n d o w . o p e n ( ' / s / c l i c k t h r o u g h . j s p ? p l i d = 5 1 6 1 2 9 7 & c p n i d = 7 0 0 9 3 3 4 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = E X P E D I A F L O A T _ U S _ C M P 2 & p r v = E X P E D I A F L O A T _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = E X P E D I A F L O A T _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = O T 0 5 k k T e D r i - S N T z n z X 6 4 T V D K M 8 & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 3 ' ) ; "   c l a s s = " p r o v i d e r i t e m " > 
 < i m g   s r c = " / v 7 0 1 / k i m g / l o g o s / d f 2 a 9 7 b 2 8 e 1 c b f 9 0 . p n g "   / > 
 < i m g   s t y l e = " b o r d e r : n o n e "   s r c = " "   w i d t h = " 1 "   h e i g h t = " 1 " / > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   o n c l i c k = " j q . n o o p ( ) ; "   t y p e = " b u t t o n " > < s p a n > C o m p a r e   & r a q u o ; < / s p a n > < / b u t t o n > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " p r o v i d e r s e c t i o n " > 
 < d i v   o n c l i c k = " w i n d o w . o p e n ( ' / s / c l i c k t h r o u g h . j s p ? p l i d = 9 6 6 0 0 6 2 & c p n i d = 7 0 1 2 0 3 7 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = O N E T R A V E L R R _ U S _ C M P 2 & p r v = O N E T R A V E L R R _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = O N E T R A V E L R R _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = w N w F E c 8 W K 6 y O S M H I _ f z q f t u 8 7 8 o & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 4 ' ) ; "   c l a s s = " p r o v i d e r i t e m " > 
 < i m g   s r c = " / v 7 0 1 / k i m g / c o m p a r e 2 / l o g o s / 9 3 1 9 b 3 9 a 9 7 c c 1 c 5 6 . j p g "   / > 
 < i m g   s t y l e = " b o r d e r : n o n e "   s r c = " "   w i d t h = " 1 "   h e i g h t = " 1 " / > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   o n c l i c k = " j q . n o o p ( ) ; "   t y p e = " b u t t o n " > < s p a n > C o m p a r e   & r a q u o ; < / s p a n > < / b u t t o n > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " p r o v i d e r s e c t i o n   p r o v i d e r a l l "   i d = " r i g h t R a i l C o m p a r e A l l " > 
 < d i v   c l a s s = " c e n t e r b t n p a d d i n g " > 
 < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   c o m p a r e A l l B u t t o n   "   t y p e = " b u t t o n " > < s p a n > C o m p a r e   a l l < / s p a n > < / b u t t o n >   < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " b a n n e r a d 2 " > < c e n t e r > 
 < b r / > 
 < s p a n   i d = " d i s p l a y A d S p a n F r a m e 2 " > 
 < d i v   b o r d e r = " 0 "   m a r g i n w i d t h = " 0 "   m a r g i n h e i g h t = " 0 "   s t y l e = " m a r g i n - t o p : 0 p x ; o v e r f l o w : h i d d e n ; "   c l a s s = " n o n d s   G P T "   i d = " d i s p l a y A d 2 " > < / d i v > 
 < / s p a n > 
 < d i v   i d = " d i s p l a y A d H i d e 2 "   c l a s s = " h i d e d i s p l a y a d "   s t y l e = " v i s i b i l i t y :   h i d d e n ; " > 
 < a   c l a s s = " h i d e d i s p l a y a d   a c t i o n l i n k "   h r e f = " j a v a s c r i p t :   h i d e D i s p l a y A d s ( t r u e ) " > 
 < s p a n   i d = " h i d e d i s p m s g 2 " > h i d e   d i s p l a y   a d s < / s p a n > 
 < / a > 
 < / d i v > 
 < / c e n t e r > 
 < / d i v > 
 < d i v   i d = " p g a C o n t a i n e r "   c l a s s = " p g a C o n t a i n e r   a j a x m a g i c   " > < / d i v > 
 < d i v   i d = " r i g h t R a i l T e x t A d s "   c l a s s = " a d S p a c e R e s u l t s W r a p p e r   n r A d s " > < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " c l e a r " > < / d i v > 
 < / d i v > 
 < / d i v >   < / d i v > 
 < / d i v > 
 < d i v   c l a s s = " p o w e r F l e x D i a l o g "   i d = " p o w e r F l e x D i a l o g " > 
 < d i v   c l a s s = " p o w e r F l e x D i a l o g M o d a l "   i d = " p o w e r F l e x D i a l o g M o d a l " > < / d i v > 
 < d i v   c l a s s = " p o w e r F l e x D i a l o g C o n t e n t " > 
 < d i v   c l a s s = " p o w e r F l e x D i a l o g I n n e r C o n t e n t " > 
 < d i v   c l a s s = " p o w e r F l e x H e a d e r " > 
 < s p a n   c l a s s = " p o w e r F l e x C l o s e " > < b u t t o n   c l a s s = " u i - b u t t o n   u i - b u t t o n - g r a y   "   i d = " p o w e r F l e x C l o s e "   o n c l i c k = " r e t u r n   R 9 . R P . F l i g h t s . P o w e r F l e x . R o u n d T r i p . t o g g l e ( j q ( ' # p o w e r F l e x T o g g l e ' ) ) "   t y p e = " b u t t o n " > < s p a n > C l o s e < / s p a n > < / b u t t o n > < / s p a n > 
 < s p a n   c l a s s = " p o w e r F l e x H e a d e r M a i n " > F l e x i b l e   d a t e s < / s p a n > 
 < / d i v > 
 < d i v   c l a s s = " p o w e r F l e x F o r m " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " p f D e p "   i d = " p f D e p "   s i z e = " 1 7 "   v a l u e = " L o s   A n g e l e s   ( L A X ) "   c l a s s = " r 9 - s m a r t y - i n p u t "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " p f D e p C o d e "   i d = " p f D e p C o d e "   v a l u e = " L A X / 1 6 0 7 8 "   / > 
 < s p a n   c l a s s = " i n l i n e a r r o w " > < / s p a n > 
 < i n p u t   t y p e = " t e x t "   n a m e = " p f D s t "   i d = " p f D s t "   s i z e = " 1 3 "   v a l u e = " B e i j i n g   ( P E K ) "   c l a s s = " r 9 - s m a r t y - i n p u t "   / > 
 < i n p u t   t y p e = " h i d d e n "   n a m e = " p f D s t C o d e "   i d = " p f D s t C o d e "   v a l u e = " P E K / 3 2 8 6 "   / > 
 < s p a n   c l a s s = " i n l i n e f i e l d s p a c e r " > < / s p a n > 
 < s p a n   c l a s s = " p f D a t e s " > 
 < s p a n   c l a s s = " p f D a t e s C o v e r " > < / s p a n > 
 < s p a n   c l a s s = " p f M i n i C a l "   i d = " p f M i n i C a l " > < / s p a n > 
 < s p a n   c l a s s = " p f D a t e W r a p p e r " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " p f D e p D a t e "   i d = " p f D e p D a t e "   s i z e = " 1 0 "   v a l u e = " 1 2 / 2 4 / 2 0 1 4 "   m a x l e n g t h = " 1 0 "   c l a s s = " d a t e P i c k e r "   / > 
 < s p a n   c l a s s = " p f P r e v B t n "   i d = " p f D e p D a t e P r e v " > < s p a n > < / s p a n > < / s p a n > 
 < s p a n   c l a s s = " p f N e x t B t n "   i d = " p f D e p D a t e N e x t " > < s p a n > < / s p a n > < / s p a n > 
 < / s p a n > 
 < s p a n   c l a s s = " i n l i n e a r r o w " > < / s p a n > 
 < s p a n   c l a s s = " p f D a t e W r a p p e r " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " p f R e t u r n D a t e "   i d = " p f R e t u r n D a t e "   s i z e = " 1 0 "   v a l u e = " 0 1 / 1 2 / 2 0 1 5 "   m a x l e n g t h = " 1 0 "   c l a s s = " d a t e P i c k e r "   / > 
 < s p a n   c l a s s = " p f P r e v B t n "   i d = " p f R e t D a t e P r e v " > < s p a n > < / s p a n > < / s p a n > 
 < s p a n   c l a s s = " p f N e x t B t n "   i d = " p f R e t D a t e N e x t " > < s p a n > < / s p a n > < / s p a n > 
 < / s p a n > 
 < s p a n   c l a s s = " p f D a t e W r a p p e r   p f T r i p L e n g t h W r a p p e r " > 
 < i n p u t   t y p e = " t e x t "   n a m e = " p f T r i p L e n g t h "   i d = " p f T r i p L e n g t h "   r e a d o n l y = " r e a d o n l y "   / > 
 < s p a n   c l a s s = " p f P r e v B t n "   i d = " p f T r i p L e n g t h P r e v " > < s p a n > < / s p a n > < / s p a n > 
 < s p a n   c l a s s = " p f N e x t B t n "   i d = " p f T r i p L e n g t h N e x t " > < s p a n > < / s p a n > < / s p a n > 
 < / s p a n > 
 < / s p a n > 
 < s p a n   c l a s s = " i n l i n e f i e l d s p a c e r " > < / s p a n > 
 < b u t t o n   c l a s s = " u i - b u t t o n   "   i d = " p f S u b m i t B t n "   t y p e = " s u b m i t " > < s p a n > F i n d   F l i g h t s < / s p a n > < / b u t t o n >   < / d i v > 
 < d i v   c l a s s = " p o w e r F l e x C h a r t H e a d e r " > B e s t   f a r e s   f o u n d   i n   t h e   p a s t   4 8   h o u r s < / d i v > 
 < d i v   i d = " p o w e r F l e x M a i n C h a r t " > < / d i v > 
 < d i v   i d = " p o w e r F l e x S m a l l C h a r t " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " p f L o a d i n g " > 
 < d i v   c l a s s = " r 9 - s p i n n e r " > < / d i v > 
 < / d i v > 
 < / d i v > 
 < d i v   i d = " f a d e r P a n e " > 
 < / d i v > 
 < d i v   i d = " p o p u p S e a r c h L a z y "   c l a s s = " l a z y " > 
 < / d i v > 
 < d i v   i d = " s c r i p t s "   s t y l e = " d i s p l a y : n o n e " > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / a l i e n / r 9 a l i e n . j s ? v = d d c 1 5 2 a 2 f 4 9 e d 7 6 5 a a 6 8 6 7 3 8 a 5 0 4 5 3 6 5 4 8 4 4 6 9 7 1 " > < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / r 9 . j s ? v = d a a 9 9 4 5 3 0 5 d 3 f c c 7 9 9 c c 6 f 9 2 e 9 f 9 2 a 7 9 b 5 e 5 7 e 3 c " > < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / s t r e a m i n g . j s ? v = 6 5 2 4 9 f 7 b 6 2 b 4 6 5 0 3 4 3 6 9 4 1 0 5 2 c 8 e 5 4 7 d b d 0 5 a 9 b 6 " > < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / f l i g h t . j s ? v = a 6 a 5 b b 5 e 5 2 9 b 7 7 3 1 5 f 5 2 b a d 8 f 6 d d d 5 5 9 e 2 d c c f a 5 " > < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / p r o g r e s s b a r t i m e r . j s ? v = d 9 6 3 f d 7 f 0 b 7 a 8 e 4 f 2 5 1 5 5 2 4 b 3 9 2 7 5 a 4 4 4 c 4 9 b 7 2 5 " > < / s c r i p t > 
� 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / r e s / e n / U S / 0 / a d s c o r e - t e m p t a t i o n / 0 / 0 / j s / p a g e r L i n k s . j s ? v = 0 e 9 2 c 2 3 6 8 6 8 d 0 9 3 9 4 d c 6 c b 0 a b 9 a a f 2 6 9 0 6 8 6 b b 3 3 " > < / s c r i p t > 
 < s c r i p t   i d = " m a i n s c r i p t "   t y p e = " t e x t / j a v a s c r i p t " > 
 s h o w R P S p i n n e r ( R 9 . g l o b a l s . m e s s a g e s . s e a r c h I n P r o g r e s s ) ; 
 v a r   g o o g l e t a g   =   { } ; 
 g o o g l e t a g . c m d   =   [ ] ; 
 ( f u n c t i o n ( )   { 
 v a r   g a d s   =   d o c u m e n t . c r e a t e E l e m e n t ( " s c r i p t " ) ; 
 g a d s . a s y n c   =   t r u e ; 
 g a d s . t y p e   =   " t e x t / j a v a s c r i p t " ; 
 v a r   u s e S S L   =   " h t t p s : "   = =   d o c u m e n t . l o c a t i o n . p r o t o c o l ; 
 g a d s . s r c   =   ( u s e S S L   ?   " h t t p s : "   :   " h t t p : " )   +   " / / w w w . g o o g l e t a g s e r v i c e s . c o m / t a g / j s / g p t . j s " ; 
 v a r   h e a d N o d e   =   d o c u m e n t . g e t E l e m e n t s B y T a g N a m e ( " h e a d " ) [ 0 ] ; 
 h e a d N o d e . a p p e n d C h i l d ( g a d s ) ; 
 } ) ( ) ;   
 R 9 . e x t e n d ( ' A d s . S m a r t A d ' ,   { 
 p o l l I n t e r v a l   :   2 0 0 , 
 i s E x p a n d e d :   f a l s e , 
 i m a g e S l o t E n a b l e d :   f a l s e 
 } ) ; 
 g o o g l e t a g . c m d . p u s h ( f u n c t i o n ( )   { 
 i f   ( t y p e o f   g o o g l e t a g   = =   ' u n d e f i n e d '   | |   t y p e o f   g o o g l e t a g . p u b a d s   = =   ' u n d e f i n e d ' )   { 
 r e t u r n ; 
 } 
 g o o g l e t a g . p u b a d s ( ) . s e t T a r g e t i n g ( ' u _ t c ' ,   ' R 1 T _ E w - A A A B S L i b 0 g Q - b e - B p T 8 u A ' ) . s e t T a r g e t i n g ( ' o c i ' ,   ' L O S _ A N G E L E S ' ) . s e t T a r g e t i n g ( ' r 0 i m a g e ' ,   ' f a l s e ' ) . s e t T a r g e t i n g ( ' x p ' ,   ' a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t ' ) . s e t T a r g e t i n g ( ' s s l ' ,   ' f a l s e ' ) . s e t T a r g e t i n g ( ' o c o ' ,   ' U S ' ) . s e t T a r g e t i n g ( ' u _ s i d ' ,   ' H - 4 1 j 6 C a 4 Q M Y T v Q t Y Y A f 2 Z G ' ) . s e t T a r g e t i n g ( ' e d ' ,   ' 1 2 ' ) . s e t T a r g e t i n g ( ' o c i d ' ,   ' L o s _ A n g e l e s ' ) . s e t T a r g e t i n g ( ' d a y s o u t ' ,   ' 7 4 ' ) . s e t T a r g e t i n g ( ' s t ' ,   ' B J ' ) . s e t T a r g e t i n g ( ' t r i p ' ,   ' r t ' ) . s e t T a r g e t i n g ( ' s y ' ,   ' 2 0 1 4 ' ) . s e t T a r g e t i n g ( ' o a c ' ,   ' L A X ' ) . s e t T a r g e t i n g ( ' d a c ' ,   ' P E K ' ) . s e t T a r g e t i n g ( ' s m ' ,   ' 1 2 ' ) . s e t T a r g e t i n g ( ' o s t ' ,   ' C A ' ) . s e t T a r g e t i n g ( ' e y ' ,   ' 2 0 1 5 ' ) . s e t T a r g e t i n g ( ' s d ' ,   ' 2 4 ' ) . s e t T a r g e t i n g ( ' i s T e s t ' ,   ' N ' ) . s e t T a r g e t i n g ( ' c i d ' ,   ' B e i j i n g ' ) . s e t T a r g e t i n g ( ' s e a r c h i d ' ,   ' J l A C B _ r Q C 4 ' ) . s e t T a r g e t i n g ( ' c o ' ,   ' C N ' ) . s e t T a r g e t i n g ( ' u _ d c t i d ' ,   ' 3 2 8 6 ' ) . s e t T a r g e t i n g ( ' o v e r r i d e ' ,   ' n ' ) . s e t T a r g e t i n g ( ' t i l e ' ,   ' 1 ' ) . s e t T a r g e t i n g ( ' c i ' ,   ' B E I J I N G ' ) . s e t T a r g e t i n g ( ' u _ o c t i d ' ,   ' 1 6 0 7 8 ' ) . s e t T a r g e t i n g ( ' e m ' ,   ' 1 ' ) . s e t T a r g e t i n g ( ' w d ' ,   ' y e s ' ) . s e t T a r g e t i n g ( ' c c ' ,   ' e ' ) ; 
 g o o g l e t a g . p u b a d s ( ) . s e t ( " p a g e _ u r l " ,   " h t t p : / / w w w . k a y a k . c o m / C h e a p - F l i g h t s - L o s - A n g e l e s - t o - B e i j i n g . L A X . P E K . c o . h t m l " ) ; 
 g o o g l e t a g . d e f i n e S l o t ( " 1 2 9 0 7 6 5 7 / k a y a k / u s / f l i g h t / r e s u l t s " ,   [ 3 0 0 , 2 5 0 ] ,   " d i s p l a y A d 1 " ) . a d d S e r v i c e ( g o o g l e t a g . p u b a d s ( ) ) . c l e a r T a r g e t i n g ( ) . s e t T a r g e t i n g ( ' p o s ' ,   ' u p p e r - r i g h t ' ) . s e t T a r g e t i n g ( ' o r i g i n ' ,   ' F . . R P . . R 0 ' ) . s e t T a r g e t i n g ( ' t i l e ' ,   ' 1 ' ) ; 
 g o o g l e t a g . d e f i n e S l o t ( " 1 2 9 0 7 6 5 7 / k a y a k / u s / f l i g h t / r e s u l t s " ,   [ 1 6 0 , 6 0 0 ] ,   " d i s p l a y A d 2 " ) . a d d S e r v i c e ( g o o g l e t a g . p u b a d s ( ) ) . c l e a r T a r g e t i n g ( ) . s e t T a r g e t i n g ( ' p o s ' ,   ' r i g h t ' ) . s e t T a r g e t i n g ( ' o r i g i n ' ,   ' F . . R P . . R 1 ' ) . s e t T a r g e t i n g ( ' t i l e ' ,   ' 2 ' ) ; 
 g o o g l e t a g . d e f i n e S l o t ( " 1 2 9 0 7 6 5 7 / k a y a k / u s / f l i g h t / r e s u l t s " ,   [ 3 0 0 , 2 5 0 ] ,   " d i s p l a y A d 3 " ) . a d d S e r v i c e ( g o o g l e t a g . p u b a d s ( ) ) . c l e a r T a r g e t i n g ( ) . s e t T a r g e t i n g ( ' p o s ' ,   ' b o t t o m ' ) . s e t T a r g e t i n g ( ' o r i g i n ' ,   ' F . . R P . . M 0 ' ) . s e t T a r g e t i n g ( ' t i l e ' ,   ' 3 ' ) ; 
 g o o g l e t a g . d e f i n e S l o t ( " 1 2 9 0 7 6 5 7 / k a y a k / u s / f l i g h t / r e s u l t s " ,   [ 1 6 0 , 6 0 0 ] ,   " d i s p l a y A d 4 " ) . a d d S e r v i c e ( g o o g l e t a g . p u b a d s ( ) ) . c l e a r T a r g e t i n g ( ) . s e t T a r g e t i n g ( ' p o s ' ,   ' l e f t ' ) . s e t T a r g e t i n g ( ' o r i g i n ' ,   ' F . . R P . . L 0 ' ) . s e t T a r g e t i n g ( ' t i l e ' ,   ' 4 ' ) ; 
 g o o g l e t a g . p u b a d s ( ) . c o l l a p s e E m p t y D i v s ( ) ; 
 g o o g l e t a g . p u b a d s ( ) . e n a b l e S i n g l e R e q u e s t ( ) ; 
 g o o g l e t a g . e n a b l e S e r v i c e s ( ) ; 
 } ) ;   
 v a r   s h o w N e w G P T A d s   =   R 9 . C o m m o n . U t i l s . d e b o u n c e ( f u n c t i o n ( ) { 
 t r y   { 
 v a r   l i m i t   =   ( 4 - 1 )   | |   1 ; 
 v a r   d i s p l a y   =   R 9 . A d s . S m a r t A d . p r e p a r e R e n d e r ( l i m i t ) ; 
 g o o g l e t a g . c m d . p u s h ( f u n c t i o n ( )   {   g o o g l e t a g . d i s p l a y ( " d i s p l a y A d 1 " ) ;   d i s p l a y ( ) ;   } ) ; 
 g o o g l e t a g . c m d . p u s h ( f u n c t i o n ( )   {   g o o g l e t a g . d i s p l a y ( " d i s p l a y A d 2 " ) ;   d i s p l a y ( ) ;   } ) ; 
 g o o g l e t a g . c m d . p u s h ( f u n c t i o n ( )   {   g o o g l e t a g . d i s p l a y ( " d i s p l a y A d 3 " ) ;   d i s p l a y ( ) ;   } ) ; 
 g o o g l e t a g . c m d . p u s h ( f u n c t i o n ( )   {   g o o g l e t a g . d i s p l a y ( " d i s p l a y A d 4 " ) ;   d i s p l a y ( ) ;   } ) ; 
 }   c a t c h   ( i g n o r e d )   { } 
 } ,   R 9 . A d s . S m a r t A d . p o l l I n t e r v a l   *   2 ) ; 
 t r y   { 
 v a r   S t a r t T a b   =   " " ; 
 v a r   i s S e o P a g e   =   f a l s e ;   v a r   i s S e m P a g e   =   f a l s e ;   
 A j a x R e g . r e m o t e H o s t   =   " h t t p s : / / w w w . k a y a k . c o m " ; 
 s e r v e r _ h t m l   =   t r u e ; 
 v a r   R 9 R s l t C o u n t   =   - 1 ; 
 v a r   R 9 F l t r d C o u n t   =   0   ; 
 v a r   R 9 P r o d u c t V e r s i o n   =   " / v R 2 2 8 b " ; 
 v a r   S e a r c h T y p e   =   " f l i g h t " ; 
 v a r   S e a r c h I D   =   " J l A C B _ r Q C 4 " ; 
 v a r   S e s s i o n I D   =   " H - 4 1 j 6 C a 4 Q M Y T v Q t Y Y A f 2 Z G " ; 
 v a r   S e r v e r U R L   =   " h t t p : / / w w w . k a y a k . c o m " ; 
 v a r   R e g i s t e r U R L   =   " h t t p s : / / w w w . k a y a k . c o m / k / i d e n t / r e g i s t e r " ; 
 v a r   S e a r c h U R L   =   " h t t p : / / w w w . k a y a k . c o m / f l i g h t s ? f i d = & a d u l t s = 1 & c a b i n = e & r e t u r n _ d a t e = 0 1 % 2 F 1 2 % 2 F 2 0 1 5 & r e t u r n _ t i m e = a & o n e w a y = n & d e p a r t _ d a t e = 1 2 % 2 F 2 4 % 2 F 2 0 1 4 & d e p a r t _ t i m e = a & o r i g i n c o d e = L A X / 1 6 0 7 8 & o r i g i n = L A X & d e s t c o d e = P E K / 3 2 8 6 & d e s t i n a t i o n = P E K & a c t i o n = d o f l i g h t s " ; 
 v a r   C o o k i e P r e f i x   =   " p 1 . m e d . " ; 
 v a r   C o o k i e D o m a i n   =   " . k a y a k . c o m " ; 
 v a r   L o g V S E v e n t s   =   t r u e ; 
 v a r   L o g S c r e e n R e s   =   f a l s e ; 
 v a r   s h o w D e t a i l s I n D i v   =   f a l s e ; 
 v a r   f o r c e D e t a i l s I n D i v   =   f a l s e ; 
 v a r   u s e N e w D e t a i l s   =   f a l s e ; 
 v a r   f r e e s e a r c h h i n t   =   ' S e a r c h   H o t e l s   ( e . g .   l a n d m a r k s ,   h o t e l   n a m e ,   u s e r   r e v i e w s ,   e t c . ) ' ; 
 v a r   f r e e s e a r c h d i r e c t i o n s   =   ' P l e a s e   e n t e r   w o r d ( s )   t o   s e a r c h   t h e s e   h o t e l s . ' ; 
 v a r   s m a r t y T y p e d e l a y   =   1 2 5 ; 
 v a r   a g e n t I s I E   =   f a l s e ; 
 v a r   a g e n t I s F F   =   f a l s e ; 
 v a r   a g e n t I s C h r o m e   =   f a l s e ; 
 v a r   i s N o n A v a i l   =   f a l s e ; 
 v a r   s h o w U n p r i c e d   =   t r u e   ; 
 C M P 2 R E Q U I R E D E S T   =   t r u e ; 
 i f   ( t y p e o f   h i d e F l i g h t D e a l s E n a b l e d   = =   " f u n c t i o n "   & &   h i d e F l i g h t D e a l s E n a b l e d ( )   = =   t r u e )   { 
 h i d e R e s u l t I n l i n e M u l t i ( ) ; 
 } 
 v a r   c u r r e n t v i e w   =   ' l i s t ' ; 
 v a r   d e s t i n a t i o n D i s p l a y   =   ' ' ; 
 d e s t i n a t i o n D i s p l a y   =   " B e i j i n g ,   C h i n a " ; 
 v a r   R 9 A n o n   =   t r u e ; 
 v a r   l e g T r i p s   =   n e w   A r r a y ( ) ; 
 / /   M o d e l   o b j e c t s   f o r   t h i s   p a g e   r e s u l t s 
 v a r   S y m R e a l W i n O p e n   =   w i n d o w . o p e n ; 
 c a n c e l A f t e r c l i c k   =   t r u e ; 
 v a r   x s e l l L i n k   =   " a c t i o n = f e t c h & d 1 = 1 2 - 2 4 - 2 0 1 4 & d 2 = 0 1 - 1 2 - 2 0 1 5 & c t i d = 3 2 8 6 & t = 1 & s r c h i d = J l A C B _ r Q C 4 "   ; 
 R 9   =   R 9   | |   { } ; 
 R 9 . e x t e n d ( ' g l o b a l s ' ,   { " p l a t f o r m " : " b b " , " a d s R e f r e s h T i m e r " : 5 0 0 0 , " i g n o r e U r l R o u t e s " : [ " ^ \ \ / ? h o t e l s . * ? - d e t a i l s \ \ / . * ? $ " ] , " l o c a l e " : { " l c _ c c " : " U S " , " l o c " : " e n _ U S " , " l c " : " e n " } , " _ R I G H T _ C U R S Y M B O L " : " " , " _ P R I C E _ T H O U S A N D S _ S E P A R A T O R " : " " , " a f f i l i a t e s k i n " : " k a y a k " , " a n a l y t i c s " : { " v e r t i c a l " : " f l i g h t " , " g o o g l e " : { " t r a c k E C o m m e r c e " : " t r u e " , " t r a c k E v e n t " : " t r u e " , " t r a c k P a g e V i e w " : " t r u e " , " t i d " : " U A - 4 2 2 0 9 1 8 5 - 8 " } , " p a g e I d " : " r e s u l t s " , " v s l o g " : { " t r a c k E v e n t " : " t r u e " , " t r a c k P a g e V i e w " : " t r u e " } } , " f a c e b o o k A p p I d " : " 1 6 3 0 0 3 0 7 9 2 0 4 " , " a f f i l i a t e " : " k a y a k " , " b r o w s e r C a p a b i l i t i e s " : { " l i m i t e d " : " f a l s e " , " p o p U n d e r M e t h o d " : " f o c u s w i n d o w n a m e " } , " f o r m t o k e n " : " r U T D i K p t k 9 8 z W V t l 9 B V R P h 1 W 0 o 4 " , " u i c o n f i g " : { " u i . s e m . h o t e l s . o v e r l a y . m v c " : " t r u e " , " u i . e r r o r . l o g g i n g . s t r e a m e r " : " t r u e " , " u i . e r r o r . l o g g i n g . s t r e a m e r . t i m e o u t " : " t r u e " , " u i . c m p 2 . m a x P r e C h e c k s " : " 2 " , " u i . c m p 2 . l i m i t P r e C h e c k s " : " f a l s e " , " u i . c m p 2 . i m p r o v e d . p a g e _ o r i g i n " : " t r u e " , " u i . c m p 2 . f o r c e U s e P r e C h e c k I t e m s " : " t r u e " , " u i . p a y m e n t f e e . m a x . b a g g a g e " : " 3 " , " u i . f l i g h t . e x t r a _ v e r t i c a l _ m u l t i b o o k " : " f a l s e " , " u i . p a y m e n t f e e . p o p o v e r . w i d t h " : " 2 6 5 " , " u i . c m p 2 . r e d u c e . c a l l s " : " t r u e " } , " e x p e r i m e n t " : " a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t " , " _ L E F T _ C U R S Y M B O L " : " $ " , " s e a r c h C o u n t " : 1 5 1 , " t r i p s a l i a s e s " : " t r i p s @ k a y a k . c o m , m y t r i p s @ k a y a k . c o m , p l a n s @ k a y a k . c o m " , " s u b m i t T i m e o u t " : 0 , " u s e D e f a u l t N a v i g a t i o n " : " t r u e " } ) ; 
 R 9 . e x t e n d ( " g l o b a l s " ,   { v e r t i c a l :   " f l i g h t " } ) ; 
 R 9 . e x t e n d ( " g l o b a l s " , { 
 u s e F u l l S e a r c h U r l :   f a l s e , 
 i s T o u c h :   j q . s u p p o r t . t o u c h   | |   t y p e o f ( w i n d o w . o n t o u c h s t a r t )   ! =   " u n d e f i n e d " , 
 i s S e o :   f a l s e } ) ; 
 R 9 . A l i e n . M o m e n t . s e t u p L o c a l e ( R 9 . g l o b a l s . l o c a l e ,   m o m e n t ,   { 
 w e e k   :   { 
 d o w   :   1   -   1 ,   d o y   :   4   } 
 } ) ; 
 R 9 . e x t e n d ( ' U R L . G e n e r a t o r . p r o t o t y p e . o p t i o n s ' ,   { 
 m a x L e g s :   6 , 
 u s e C l e a n U r l s :   t r u e , 
 u s e M o b i l e U r l s :   f a l s e 
 } ) ; 
 R 9 . e x t e n d ( ' R P . I n l i n e F o r m . p r o t o t y p e . o p t i o n s ' ,   { 
 m e s s a g e :   " S t a r t i n g   n e w   s e a r c h . . . " 
 } ) ; 
 R 9 . V i e w S t a t e . g e t I n s t a n c e ( { " v i e w S t a t e s K e y s " : { " s h o w L a y o v e r F i l t e r " : " l a y " , " m a p V i e w P o r t C e n t e r " : " m v p c " , " h o t e l M a p M i n i D e t a i l s I d " : " h m m d " , " c a r D e t a i l s O p e n e d I n d e x e s " : " c o d i " , " l a s t C l i c k e d R e s u l t I d K e y " : " l t " , " h o t e l M a p F i l t e r O p e n e d I d " : " h m f o i d " , " c l i c k e d R e s u l t I d L i s t K e y " : " t " , " h o t e l D e t a i l s O p e n e d I n d e x " : " h d o i x " , " f l i g h t D e t a i l s O p e n e d I n d e x e s " : " f o d i " , " h o t e l D e t a i l s O p e n e d T a b " : " h d o t " , " m u l t i C i t y F i l t e r S e c t i o n L e g " : " m c l e g " , " r e s u l t P a g e N u m b e r " : " p n " , " f l i g h t F l e x G r i d " : " f f l x " , " w i n d o w S c r o l l " : " w s " , " h o t e l M i n i D e t a i l s O p e n e d T a b " : " h m d o t " , " h o t e l M i n i D e t a i l s O p e n e d I d P a g e " : " h m d o i p " , " h o t e l M i n i D e t a i l s O p e n e d I d " : " h m d o i d " , " a i r l i n e F i l t e r I s A i r l i n e V i e w " : " f a f a " , " m a p Z o o m L e v e l " : " m z l " , " s h o w M a t r i x " : " m t x " , " h o t e l D e t a i l s O p e n e d I d " : " h d o i d " , " l a n d i n g T i m e S l i d e r s " : " l t s " , " c h o s e n D e p a r t u r e I n d e x " : " c d i d x " } , " v e r t i c a l " : " f l i g h t s " , " u r l S t a t e R e q u e s t K e y " : " u r l V i e w S t a t e " , " s e a r c h B a s e U r l " : " L A X - P E K / 2 0 1 4 - 1 2 - 2 4 / 2 0 1 5 - 0 1 - 1 2 / " , " c u r r e n t U r l S t a t e " : " " , " s a v i n g " : { " r e q u e s t K e y " : " v i e w S t a t e " , " r e s e t V a l u e " : " R E S E T " } , " s t a t e D e l i m i t e r " : " ; " , " u r l V i e w S t a t e K e y s " : { " m a p T y p e I d " : " m t i d " , " r e s u l t V i e w M o d e " : " v w " } } ) . r e s t o r e U r l S t a t e s ( { } ) ; 
 A j a x R e g . r e m o t e H o s t   =   " h t t p s : / / w w w . k a y a k . c o m " ; 
 R 9 . e x t e n d ( ' c m p 2 ' ,   { 
 u s e I n t e r s t i t i a l P a g e :   t r u e , 
 j a v a s c r i p t R e f a c t o r :   t r u e , 
 s u p p r e s s K A Y A K C o m p a r e T o P o p u p :   t r u e   , 
 n o O p e n P o p u n d e r s :   f a l s e , 
 u s e R e s e r v e W i n d o w s :   f a l s e , 
 p o p u p B l o c k e r M e s s a g e E n a b l e d :   t r u e , 
 q u e u e P o p u p s :   t r u e , 
 t i l e W i n d o w s :   t r u e , 
 s a v e P o s i t i v e M e t a C o o k i e s :   t r u e , 
 s a v e N e g a t i v e M e t a C o o k i e s :   t r u e 
 } ) ; 
 R 9 . e x t e n d ( ' c c f e e c a l c u l a t o r ' ,   { 
 a t t a c h C C F e e C a l c N o t i c e T o o l t i p :   f a l s e , 
 e n a b l e B a g g a g e F e e s :   f a l s e 
 } ) ; 
 R 9 . e x t e n d ( ' l o g i n ' ,   { 
 n e w f o r m :   f a l s e 
 } ) ; 
 R 9 . e x t e n d ( ' T a b s . S e a r c h T a b s . p r o t o t y p e . o p t i o n s ' ,   { 
 m e s s a g e :   " L o a d i n g   r e s u l t s . . . " 
 } ) ; 
 R 9 . e x t e n d ( ' C o u n t r y C u r r e n c y P i c k e r ' ,   { 
 ' c o n f i g '   :   { 
 ' s p a r k l e . a f f i l i a t e . c o u n t r y p i c k e r . c u r r e n c y . s h o w S a v e T o P r o f i l e D i a l o g '   :   t r u e , 
 ' n u c l e u s . i s p f i l t e r . n e w R e d i r e c t '   :   t r u e 
 } 
 } ) ; 
 w i n d o w . R 9 A p p   =   w i n d o w . R 9 A p p   | |   { } ; 
 i f   ( ! w i n d o w . R 9 A p p . A p p I n s t a n c e )   { 
 v a r   a p p O p t i o n s   =   { 
 u s e C l i e n t C a c h e :   ( " f a l s e "   ! = =   R 9 . g l o b a l s . u s e C l i e n t C a c h e ) , 
 c a c h e I n i t i a l P a g e :   ( " t r u e "   = = =   R 9 . g l o b a l s . c a c h e I n i t i a l P a g e ) , 
 u s e D e f a u l t N a v i g a t i o n :   ( " t r u e "   = = =   R 9 . g l o b a l s . u s e D e f a u l t N a v i g a t i o n ) , 
 m a x C a c h e E n t r y S i z e :   1 0 , 
 m a x C a c h e T T L :   1 5 * 6 0 * 1 0 0 0 , 
 i g n o r e U r l s :   R 9 . g l o b a l s . i g n o r e U r l R o u t e s   | |   [ ] 
 } ; 
 w i n d o w . R 9 A p p . A p p I n s t a n c e   =   n e w   R 9 . A p p . A p p l i c a t i o n ( a p p O p t i o n s ) ; 
 / /   c a c h e   t h e   p a g e   a t   t h e   e n d   o f   s t r e a m i n g 
 i f   ( a p p O p t i o n s . u s e C l i e n t C a c h e   & &   a p p O p t i o n s . c a c h e I n i t i a l P a g e )   { 
 j q ( w i n d o w ) . b i n d ( ' r 9 . s t r e a m i n g . d o n e ' ,   f u n c t i o n ( )   { 
 w i n d o w . R 9 A p p . A p p I n s t a n c e . c a c h e P a g e M e r g e ( d o c u m e n t . l o c a t i o n . p a t h n a m e ) ; 
 } ) ; 
 } 
 R 9 . A n a l y t i c s . a p i . t r a c k P a g e V i e w ( ) ; 
 } 
 j q ( f u n c t i o n ( )   { 
 j q ( " # h e a d e r m a i n t a b s   a " ) . n o t ( j q ( " a . d i r e c t L i n k " ) ) . o f f ( " c l i c k . n a v i g a t i o n " ) . o n ( " c l i c k . n a v i g a t i o n " ,   f u n c t i o n ( e v e n t )   { 
 e v e n t . p r e v e n t D e f a u l t ( ) ; 
 R 9 A p p . A p p I n s t a n c e . n a v i g a t e ( t h i s . p a t h n a m e   = = =   " "   ?   " / "   :   t h i s . p a t h n a m e ) ; 
 r e t u r n   f a l s e ; 
 } ) ; 
 i n i t i a l i z e C o u n t r y P i c k e r ( ) ; 
 } ) ; 
 f u n c t i o n   s e l e c t B u t t o n C l i c k T r a c k i n g E v e n t ( )   { 
 } 
 R 9 . S o c i a l . F B . v e r i f y L o g i n S t a t u s ( ) ; 
 R 9 . S o c i a l . G o o g l e . v e r i f y L o g i n S t a t u s ( ) ; 
 _ c u r r e n t s o r t i d =   " p r i c e "   ; 
 _ l a s t d i r e c t i o n = t r u e ; 
 v a r   s e a r c h i d   =   " J l A C B _ r Q C 4 " ; 
 v a r   e x p e r i m e n t   =   ' a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t ' ; 
 w i n d o w . S t r e a m i n g   =   S t r e a m e r (   f a l s e   , 
 1 4 1 3 0 6 6 9 3 5 6 3 3   , 
 6 0 0 0 0 , 
 " J l A C B _ r Q C 4 " ) ; 
 S t r e a m i n g . s t a r t ( 2 5 0 ) ; 
 v a r   s m a r t A d P r o v i d e r   =   " " ; 
 v a r   _ _ s t   =   n u l l ;   / /   s e r v e r   t i m e   s e t   o n   t h e   t a i l   e n d   o f   t h e   p a g e   l o a d 
 f u n c t i o n   s t a r t u p ( ) 
 { 
 i f   ( t y p e o f   R 9 S t a r t P a g e   ! =   ' u n d e f i n e d ' )   { 
 R 9 L o a d F i n i s h e d   =   n e w   D a t e ( ) ; 
 } 
 } 
 v a r   R A N D O M _ U P S E L L _ C O D E   =   " P A R A L Y Z E D " ; 
 v a r   S H O W _ A F T E R C L I C K _ S E C T I O N _ I N _ P O P U P   =   t r u e ; 
 v a r   S H O W _ A F T E R C L I C K _ S E C T I O N _ W H E N _ N O _ P O P U P   =   t r u e ; 
 v a r   S H O W _ A F T E R C L I C K _ S E C T I O N _ W H E N _ P O P U P   =   t r u e ; 
 S H O W _ A F T E R C L I C K _ S E C T I O N _ I N _ P O P U P   =   f a l s e ; 
 f u n c t i o n   i n i t C a l e n d a r ( )   { 
 R 9 . d p . d o u b l e P i c k e r ( 
 { 
 a l t F i e l d :   " # d e p a r t _ d a t e _ d o m l a b e l " , 
 d e f a u l t D a t e :   j q . r 9 d a t e p i c k e r . p a r s e D a t e ( " 1 2 / 2 4 / 2 0 1 4 " ) , 
 o n S e l e c t :   f u n c t i o n ( )   {   c o m p a r e 2 c h k ( ) ;   } , 
 s e l e c t o r :   " # d e p a r t _ d a t e " 
 } , 
 { 
 a l t F i e l d :   " # r e t u r n _ d a t e _ d o m l a b e l " , 
 d e f a u l t D a t e :   j q . r 9 d a t e p i c k e r . p a r s e D a t e ( " 1 / 1 2 / 2 0 1 5 " ) , 
 o n S e l e c t :   f u n c t i o n ( )   {   c o m p a r e 2 c h k ( ) ;   } , 
 s e l e c t o r :   " # r e t u r n _ d a t e " 
 } 
 ) ; 
 } 
 v a r   _ A L L R E S U L T S L O A D E D   =   t r u e ; 
 / *   S e t u p   t i m e r   t o   f i r e   t h a t   b o o k i n g   l i n k s   h a v e   e x p i r e d   * / 
 R 9 . R P . R e s u l t s E x p i r e d . s t a r t ( { 
 a c t i o n P a r a m s :   " a c t i o n = b o o k i n g l i n k s e x p i r e d & p = f l i g h t & s e a r c h i d = J l A C B _ r Q C 4 & s e a r c h u r l = h t t p % 3 A % 2 F % 2 F w w w . k a y a k . c o m % 2 F f l i g h t s % 3 F f i d % 3 D % 2 6 a d u l t s % 3 D 1 % 2 6 c a b i n % 3 D e % 2 6 r e t u r n _ d a t e % 3 D 0 1 % 2 5 2 F 1 2 % 2 5 2 F 2 0 1 5 % 2 6 r e t u r n _ t i m e % 3 D a % 2 6 o n e w a y % 3 D n % 2 6 d e p a r t _ d a t e % 3 D 1 2 % 2 5 2 F 2 4 % 2 5 2 F 2 0 1 4 % 2 6 d e p a r t _ t i m e % 3 D a % 2 6 o r i g i n c o d e % 3 D L A X % 2 F 1 6 0 7 8 % 2 6 o r i g i n % 3 D L A X % 2 6 d e s t c o d e % 3 D P E K % 2 F 3 2 8 6 % 2 6 d e s t i n a t i o n % 3 D P E K % 2 6 a c t i o n % 3 D d o f l i g h t s " , 
 t i m e o u t M S :   7 2 0 0 0 0 0 , 
 a c t i o n T y p e :   ' m o u s e r e f r e s h ' , 
 r e l o a d M e s s a g e :   ' R e s u l t s   h a v e   e x p i r e d .   W e   a r e   n o w   r e f r e s h i n g   t h e m . ' 
 } ) ; 
 w i n d o w . R 9   =   d e e p E x t e n d ( w i n d o w . R 9 , 
 { 
 t t i p :   { 
 C L :   {   p a r a m s :   {   w :   2 2 5   }   } , 
 F L :   {   p a r a m s :   {   w :   2 5 5   }   } , 
 S H R :   {   e m p t y :   1   } , 
 D L :   {   e m p t y :   1   } 
 } 
 } ) ; 
 j q ( w i n d o w ) . b i n d ( ' l o a d ' ,   s t a r t u p ) ; 
 i s F l e x D a t e   =   i s F l y N o w   =   i s W e e k e n d   =   i s O p e n F l e x   =   f a l s e ; 
 _ t r a v e l e r s   =   1 ; 
 i s F l e x D a t e   =   f a l s e ; 
 c u r r e n t v i e w   =   " l i s t " ; 
 d o w   =   [ " S u n " ,   " M o n " ,   " T u e " ,   " W e d " ,   " T h u " ,   " F r i " ,   " S a t " ] ; 
 f u n c t i o n   g e t T r i p T y p e ( ) 
 { 
 r e t u r n   " r o u n d t r i p " ; 
 } 
 f u n c t i o n   g e t N u m L e g s ( )   {   r e t u r n   2 ;   } 
 f u n c t i o n   p r o d u c t s t a r t u p ( ) 
 { 
 j q . c o o k i e ( " p 1 . m e d . r 9 O r i g i n " ,   n u l l ,   { p a t h :   ' / ' ,   s e c u r e :   f a l s e ,   d o m a i n :   " . k a y a k . c o m " } ) ;   j q . c o o k i e ( " p 1 . m e d . r 9 O r i g i n " ,   " L A X " ,   { p a t h :   ' / ' ,   s e c u r e :   f a l s e ,   e x p i r e s :   9 9 9 9 } ) ;   
 u p d a t e D i s p l a y ( 0 ,   0 ,   " p r i c e " ,   " t r u e " ,   " 1 " ) ; 
 j q ( " # f l i g h t V i e w C o n t r o l s " ) . r a d i o b u t t o n s e t ( ) ; 
 j q ( " # s h o w P r i c e M a t r i x " ) . o f f ( " c l i c k " ) . c l i c k ( f u n c t i o n ( )   { 
 t o g g l e P r i c e M a t r i x ( ) ; 
 } ) ; 
 j q ( " # s h o w P l u s M i n u s T h r e e " ) . o f f ( " c l i c k " ) . c l i c k ( f u n c t i o n ( )   { 
 t o g g l e P l u s M i n u s T h r e e ( ) ; 
 } ) ; 
 w i n d o w . s e t T i m e o u t ( f u n c t i o n ( )   { 
 w i n d o w . s c r o l l T o ( 0 ,   0 ) ; 
 } ,   1 ) ; 
 } 
 f u n c t i o n   t o g g l e P r o v i d e r C h e c k b o x ( c h e c k b o x d i v i d )   { 
 v a r   c h e c k b o x   =   j q ( " # " + c h e c k b o x d i v i d + "   >   i n p u t " ) ; 
 v a r   i s c h e c k e d   =   c h e c k b o x . p r o p ( " c h e c k e d " ) ; 
 i f ( i s c h e c k e d )   { 
 c h e c k b o x . r e m o v e P r o p ( " c h e c k e d " ) ; 
 }   e l s e   { 
 c h e c k b o x . p r o p ( " c h e c k e d " ,   t r u e ) ; 
 } 
 } 
 R 9 . e x t e n d ( ' R P . P r i c e A n d S e l e c t B u t t o n ' ,   { 
 a u t o S e l e c t F i r s t :   t r u e , 
 b i n d T o M u l t i b o o k :   f u n c t i o n ( )   { 
 } 
 } ) ; 
 c o m p l e t e I n i t i a l L o a d ( ) ; 
 u s e P o p o v e r P a g i n g T o o l t i p   =   t r u e ; 
 j q ( f u n c t i o n ( )   { 
 s e t u p P B a r ( " 6 0 0 0 0 " ) ; 
 a t t a c h I n l i n e M u l t i b o o k ( ) ; 
 i n l i n e M o d i f y S e a r c h I n i t i a l i z e ( " m m / d d / y y " , 
 " 1 2 / 2 4 / 2 0 1 4 " , 
 " 0 1 / 1 2 / 2 0 1 5 " ) ; 
 w i n d o w . c r e a t e S e a r c h M o d e l   & &   c r e a t e S e a r c h M o d e l ( " m m / d d / y y " , 
 " 1 2 / 2 4 / 2 0 1 4 " , 
 " 0 1 / 1 2 / 2 0 1 5 " ) ; 
 w i n d o w . s e t T i m e o u t ( f u n c t i o n   ( )   { 
 j q ( " < ! - -   B l u e   K a i   - - > \ n < i f r a m e   n a m e = \ " _ _ b k f r a m e \ "   h e i g h t = \ " 0 \ "   w i d t h = \ " 0 \ "   f r a m e b o r d e r = \ " 0 \ "   s r c = \ " j a v a s c r i p t : v o i d ( 0 ) \ " > < \ / i f r a m e > \ n < s c r i p t > \ n   j q . g e t S c r i p t ( \ " h t t p s : \ / \ / t a g s . b k r t x . c o m \ / j s \ / b k - c o r e t a g . j s \ " ,   f u n c t i o n ( )   { \ n   \ / \ /   m a k e   s u r e   t h e   b l u e   k a i   c o d e   l o a d e d   b e f o r e   p r o c e s s i n g   c a l l b a c k \ n   i f   ( t y p e o f ( b k _ a d d P a g e C t x )   = = =   \ " f u n c t i o n \ "   & &   t y p e o f ( b k _ d o J S T a g )   = =   \ " f u n c t i o n \ " )   { \ n   b k _ a d d P a g e C t x ( \ " P r o d u c t \ " ,   \ " f l i g h t \ " ) ; \ n   b k _ a d d P a g e C t x ( \ " C l a s s \ " ,   \ " E c o n o m y \ " ) ; \ n   b k _ a d d P a g e C t x ( \ " D e p a r t D a t e \ " ,   \ " D e c e m b e r   2 4   2 0 1 4 \ " ) ; \ n   b k _ a d d P a g e C t x ( \ " D e p a r t u r e C i t y \ " ,   \ " L A X \ " ) ; \ n   b k _ a d d P a g e C t x ( \ " D e s t i n a t i o n \ " ,   \ " P E K \ " ) ; \ n   b k _ a l l o w _ m u l t i p l e _ c a l l s = t r u e ; \ n   b k _ u s e _ m u l t i p l e _ i f r a m e s = t r u e ; \ n   b k _ s e n d _ s t a t i d _ p a y l o a d = t r u e ; \ n \ n   b k _ d o J S T a g ( 8 3 ,   6 ) ; \ n   } \ n   } ) ; \ n < \ / s c r i p t > \ n < ! - -   E n d   B l u e   K a i   - - > \ n " ) . a p p e n d T o ( " b o d y " ) ; 
 j q ( " < d i v > < i f r a m e   s t y l e = \ " v i s i b i l i t y : h i d d e n ;   w i d t h : 0 p x ;   h e i g h t : 0 p x ;   d i s p l a y : n o n e ; \ "   c l a s s = \ " l o o k b a c k U r l \ " > < \ / i f r a m e > < \ / d i v > " ) . a p p e n d T o ( " b o d y " ) ; 
 j q . g e t S c r i p t ( " h t t p : \ / \ / p i x e l . m a t h t a g . c o m \ / e v e n t \ / j s ? m t _ i d = 2 2 2 1 9 5 & m t _ a d i d = 1 1 0 2 1 6 & v 1 = 1 6 0 7 8 & v 2 = 3 2 8 6 & v 3 = F L I G H T S & v 4 = 2 0 1 4 1 2 2 4 & v 5 = 2 0 1 5 0 1 1 2 & s 1 = z J 5 R m S Z M u H 0 Q 8 s F l 1 P 7 _ X c j V M N 4 & s 2 = L o s + A n g e l e s & s 3 = B e i j i n g & s 4 = C a l i f o r n i a % 2 C U n i t e d + S t a t e s & s 5 = B e i j i n g % 2 C C h i n a & s 6 = L A X & s 7 = P E K & s 8 = k a y a k . c o m " ) ; 
 j q . g e t S c r i p t ( " h t t p s : \ / \ / p i x e l . s o j e r n . c o m \ / p i x e l \ / p a r t n e r \ / 2 e j G F w 3 S w j W k c V h h \ / f s ? t = 1 & f c = e & f a 1 = L A X & f a 2 = P E K & f d 1 = 2 0 1 4 - 1 2 - 2 4 & f d 2 = 2 0 1 5 - 0 1 - 1 2 " ) ; 
 j q . g e t S c r i p t ( " h t t p : \ / \ / l o a d u s . e x e l a t o r . c o m \ / l o a d \ / ? r e t = 1 & c t r y d = C N & c a r = 0 & g = 0 0 1 & n p = 1 & c = 2 0 2 1 1 & s c = c & t d = P E K & j = d & t o a = L A X & t i = 1 & w o y = 4 1 1 4 & p r o d u c t = f l i g h t & d u r = 6 m o r e & d d t = 1 5 0 1 1 2 & p = 5 4 2 & h o t = 0 & c t r y o = U S & a d t = 1 4 1 2 2 4 & n l = 1 " ) ; 
 j q . g e t S c r i p t ( " h t t p : \ / \ / t a g . y i e l d o p t i m i z e r . c o m \ / p s \ / p s ? t = s & p = 1 1 1 6 & u = R 1 T _ E w - A A A B S L i b 0 g Q - b e - B p T 8 u A & s f t o a c = L A X & p g = f t r s l & s f t r d t = 2 0 1 5 - 0 1 - 1 2 & s f t d p d t = 2 0 1 4 - 1 2 - 2 4 & s f t c a b c l = E c o n o m y & s f t d a c = P E K " ) ; 
 } ,   3 0 0 0 ) ; 
 v a r   p m   =   { ' b b ' :   ' D e s k t o p ' ,   ' m b '   :   ' M o b i l e ' ,   ' t b '   :   ' T a b l e t ' } ; 
 w i n d o w . I n t e n t M e d i a P r o p e r t i e s   =   { 
 p a g e _ i d :   " f r p . s k i p o n p a g e a d s " , 
 t r a v e l _ d a t e _ s t a r t :   " 2 0 1 4 - 1 2 - 2 4 " , 
 t r a v e l _ d a t e _ e n d :   " 2 0 1 5 - 0 1 - 1 2 " ,   a d u l t s :   " 1 " , 
 c h i l d r e n :   " 0 " , 
 t r a v e l e r s :   " 1 " , 
 f l i g h t _ o r i g i n :   " L A X " , 
 f l i g h t _ d e s t i n a t i o n :   " P E K " , 
 f l i g h t _ t r i p _ t y p e :   " r o u n d t r i p "   , 
 s i t e _ l a n g u a g e :   R 9 . g l o b a l s . l o c a l e . l c   | |   " e n " , 
 s i t e _ c o u n t r y :   R 9 . g l o b a l s . l o c a l e . l c _ c c   | |   " U S " , 
 s h o w _ a d s :   ' N ' , 
 s h o w _ e x i t _ u n i t s :   ' Y ' , 
 d e v i c e _ f o r m a t _ t y p e :   p m [ R 9 . g l o b a l s . p l a t f o r m ]   | |   " D e s k t o p " 
 } ; 
 i f   ( ! ( w i n d o w . I n t e n t M e d i a   & &   w i n d o w . I n t e n t M e d i a . E v e n t ) )   { 
 s e t T i m e o u t ( f u n c t i o n ( )   { 
 j q . g e t S c r i p t ( " \ / \ / a . c d n . i n t e n t m e d i a . n e t \ / j a v a s c r i p t s \ / i n t e n t _ m e d i a _ k a y a k . j s " ,   f u n c t i o n ( )   { 
 w i n d o w . l o a d I n t e n t M e d i a A d s   & &   l o a d I n t e n t M e d i a A d s ( ) ; 
 } ) ; 
 } ,   3 0 0 0 ) ; 
 } 
 j q ( ' . r 9 - d a t e p i c k e r - w r a p p e r ' ) . d a t e r a n g e P i c k e r ( ) ; 
 ; 
 ( f u n c t i o n   ( )   { 
 v a r   c m p 2 s   =   [   ] ,   c l o s e r s   =   [   ] ; 
 v a r   s h o u l d O p e n W i n d o w s   =   ! f a l s e   | |   ( f a l s e   & &   t y p e o f   o v e r r i d e C o m p a r e T o U r l s   ! = =   ' u n d e f i n e d ' ) ; 
 i f   ( s h o u l d O p e n W i n d o w s )   { 
 ( n e w   R 9 . C o m p a r e T o . W i n d o w . O p e n e r ( ) ) . o p e n ( c m p 2 s ,   t r u e ) ; 
 } 
 j q . e a c h ( c l o s e r s ,   f u n c t i o n   ( n ,   c o d e )   { 
 t r y   { 
 ( n e w   R 9 . C o m p a r e T o . W i n d o w . O p e n e r ( ) ) . c l o s e ( c o d e ) ; 
 }   c a t c h   ( e )   { 
 } 
 } ) ; 
 ( n e w   R 9 . C o m p a r e T o . W i n d o w . O p e n e r ( ) ) . r e s e t ( ) ; 
 w i n d o w . f o c u s ( ) ; 
 } ) ( ) ; 
 } ) ; 
 R 9 . g l o b a l s . f i r s t P h a s e D o n e   =   f a l s e ; 
 R 9 . g l o b a l s . s e a r c h D o n e   =   f a l s e ; 
 _ _ s t   =   1 4 1 3 0 6 6 9 3 5 8 9 9 ; 
 j q ( f u n c t i o n ( ) { 
 v a r   c m p 2 s   =   [ ] ; 
 c m p 2 s . p u s h ( {   u r l :   ' \ / s \ / c l i c k t h r o u g h . j s p ? p l i d = 8 2 5 0 9 1 3 & c p n i d = 5 0 0 6 3 2 7 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 & p r v = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = C H E A P O 2 F L O A T I N G _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = v 8 1 O Z v k d y i U l U W U Y B - r x G - o 8 4 I Y & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 1 ' ,   n a m e :   ' R i g h t R a i l s C H E A P O 2 F L O A T I N G '   } ) ; 
 c m p 2 s . p u s h ( {   u r l :   ' \ / s \ / c l i c k t h r o u g h . j s p ? p l i d = 5 1 6 1 2 3 9 & c p n i d = 7 0 0 9 2 3 9 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = P R I C E L I N E _ F L O A T _ U S _ C M P 2 & p r v = P R I C E L I N E _ F L O A T _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = P R I C E L I N E _ F L O A T _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = Z g H r N Z w k e A W N z A 2 f M t 0 y F f Y r B D E & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 2 ' ,   n a m e :   ' R i g h t R a i l s P R I C E L I N E '   } ) ; 
 c m p 2 s . p u s h ( {   u r l :   ' \ / s \ / c l i c k t h r o u g h . j s p ? p l i d = 5 1 6 1 2 9 7 & c p n i d = 7 0 0 9 3 3 4 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = E X P E D I A F L O A T _ U S _ C M P 2 & p r v = E X P E D I A F L O A T _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = E X P E D I A F L O A T _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = O T 0 5 k k T e D r i - S N T z n z X 6 4 T V D K M 8 & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 3 ' ,   n a m e :   ' R i g h t R a i l s E X P E D I A F L O A T '   } ) ; 
 c m p 2 s . p u s h ( {   u r l :   ' \ / s \ / c l i c k t h r o u g h . j s p ? p l i d = 9 6 6 0 0 6 2 & c p n i d = 7 0 1 2 0 3 7 & c t y p = S e a r c h & p t y p = F & o r i g = F . . R P . . R 0 & o c t i d = & p i d = O N E T R A V E L R R _ U S _ C M P 2 & p r v = O N E T R A V E L R R _ U S _ C M P 2 & s r c h = J l A C B _ r Q C 4 & p l o c = U S & l i d = O N E T R A V E L R R _ U S _ C M P 2 - J l A C B _ r Q C 4 & c 2 c = & x p = a a - 1 d a y , a d s c o r e - t e m p t a t i o n , b o b - b o o s t - p o p s c o r e - b u c k e t s - c , h e l i v i e w - e f f e c t & q a d u l t s = 1 & q r o o m s = 0 & q t r a v e l e r s = 1 & q o r i g = A i r p o r t : L A X & q d e s t = A i r p o r t : P E K & q s t a r t = 1 4 1 9 3 9 7 2 0 0 0 0 0 & q e n d = 1 4 2 1 0 3 8 8 0 0 0 0 0 & q s h o u r = - 1 & q e h o u r = - 1 & q o w = f a l s e & q f c c = e & q d c t i d = 3 2 8 6 & q d a c = P E K & r e s i d = & b o o k i d = & q n s = f a l s e & q n e a r b y = 0 & q n e a r b y o = f a l s e & q n e a r b y d = f a l s e & q c a g e s = & q i n f a n t s e a t = 0 & q i n f a n t l a p = 0 & q s e n i o r = 0 & q d a c = P E K & q o a c = L A X & x p E x t = & a i d E x t = & a d t y p e = c m p 2 & d i s p l a y R a i l = r i g h t & h = w N w F E c 8 W K 6 y O S M H I _ f z q f t u 8 7 8 o & a i d = k a y a k & l o c a l e = C o u n t r y % 3 D U S % 0 A L a n g u a g e % 3 D e n % 0 A C u r r e n c y % 3 D U S D & r a i l s i z e = 1 5 & r a n k = 4 ' ,   n a m e :   ' R i g h t R a i l s O N E T R A V E L R R '   } ) ; 
 n e w   R 9 . C o m p a r e T o . C o n t r o l l e r ( n e w   R 9 . C o m p a r e T o . V i e w . R i g h t R a i l s ( d o c u m e n t . b o d y ,   " # c m p 2 t o p r i g h t r a i l c o n t a i n e r   b u t t o n . c o m p a r e A l l B u t t o n " ,   c m p 2 s ) ,   n e w   R 9 . C o m p a r e T o . M o d e l ( ) ) ; 
 } ) ; 
 ( f u n c t i o n ( $ )   { 
 $ ( ' # p o w e r F l e x T o g g l e W r a p p e r ' ) . t i p T i p ( ) ; 
 $ . e x t e n d ( R 9 . R P . F l i g h t s . P o w e r F l e x . R o u n d T r i p . p r o t o t y p e . o p t i o n s ,   { 
 t r i p L e n g t h L a b e l :   ' d a y s ' , 
 s u b m i t B u t t o n T i t l e :   ' S e a r c h   n o w ' , 
 a i r l i n e P r e p o s i t i o n T e x t :   ' o n ' , 
 i n i t i a l D a t e P i c k e r F o r m a t :   ' m m / d d / y y ' 
 } ) ; 
 $ . e x t e n d ( R 9 . W i d g e t s . P o w e r F l e x . p r o t o t y p e . o p t i o n s ,   { 
 d a y N a m e s :   ' S , M , T , W , T , F , S ' . s p l i t ( ' , ' ) , 
 f i r s t D a y :   p a r s e I n t ( ' 1 ' ,   1 0 )   -   1 , 
 m o n t h N a m e s :   ' J a n u a r y , F e b r u a r y , M a r c h , A p r i l , M a y , J u n e , J u l y , A u g u s t , S e p t e m b e r , O c t o b e r , N o v e m b e r , D e c e m b e r ' . s p l i t ( ' , ' ) , 
 s h o r t M o n t h N a m e s :   ' J a n , F e b , M a r , A p r , M a y , J u n , J u l , A u g , S e p , O c t , N o v , D e c ' . s p l i t ( ' , ' ) 
 } ) ; 
 } ) ( w i n d o w . j q   | |   w i n d o w . j Q u e r y ) ; 
 }   c a t c h   ( e )   { 
 } 
 j q ( " # f i l t e r b l o c k " ) . c s s ( " m i n - h e i g h t " ,   j q ( " # r e s b o d y " ) . o u t e r H e i g h t ( ) ) ; 
 ( f u n c t i o n ( )   { 
 " u s e   s t r i c t " ; 
 v a r   i m p r e s s i o n M a p D a t a   =   { } ;   / /   W i l l   h o l d   i m p r e s s i o n   u r l s 
 v a r   a d S e n s e R a i l C o n f i g O p t i o n s   =   n e w   A r r a y ( ) ; 
 v a r   a d b l o c k B o t t o m R a i l C o n f i g   =   { 
 ' n u m b e r ' :   4 , 
 ' l i n e s ' :   3 , 
 ' c l i c k t r a c k U r l ' :   ' h t t p : / / w w w . k a y a k . c o m / h / a d s / t a c ? s e a r c h i d = J l A C B _ r Q C 4 & t e r m s = B e i j i n g + c h e a p + f l i g h t s & p t y p e = F & t e s t = f a l s e & a = k a y a k & a t a g = & t a x p = a a - 1 d a y % 2 C a d s c o r e - t e m p t a t i o n % 2 C b o b - b o o s t - p o p s c o r e - b u c k e t s - c % 2 C h e l i v i e w - e f f e c t & l o c = e n _ U S & o l o c = e n _ U S & t l d = o t h e r & r a i l = b o t t o m & o r i g i n = F . . R P . . M 1 & r a i l S i z e = 4 & a d t y p e = t e x t / w i d e ' , 
 ' f o n t S i z e T i t l e ' :   1 5 , 
 ' c o l o r T i t l e L i n k ' :   ' # 1 D 6 D C F ' , 
 ' c o l o r D o m a i n L i n k ' :   ' # E E 7 A 0 0 ' , 
 ' c o l o r T e x t ' :   ' # 4 4 4 ' , 
 ' n o T i t l e U n d e r l i n e ' :   t r u e , 
 ' w i d t h ' :   j q ( " # b o t t o m a d s _ t e x t _ a d s _ t d " ) . w i d t h ( ) , 
 ' c o n t a i n e r ' :   ' b o t t o m a d s _ t e x t _ a d s _ t d ' 
 } ; 
 a d S e n s e R a i l C o n f i g O p t i o n s . p u s h ( a d b l o c k B o t t o m R a i l C o n f i g ) ; 
 i m p r e s s i o n M a p D a t a [ " b o t t o m a d s _ t e x t _ a d s _ t d " ]   =   " & b o t t o m = F . . R P . . M 1 | t e x t \ / w i d e | " ; 
 f u n c t i o n   g o o g l e T e x t A d C o u n t C a l l b a c k ( n u m A d s M a p )   { 
 v a r   b a s e U R L   =   " h t t p : / / w w w . k a y a k . c o m / h / a d s / t a i ? s e a r c h i d = J l A C B _ r Q C 4 & t e r m s = B e i j i n g + c h e a p + f l i g h t s & p t y p e = F & t e s t = f a l s e & a = k a y a k & a t a g = & t a x p = a a - 1 d a y % 2 C a d s c o r e - t e m p t a t i o n % 2 C b o b - b o o s t - p o p s c o r e - b u c k e t s - c % 2 C h e l i v i e w - e f f e c t & l o c = e n _ U S & o l o c = e n _ U S & t l d = o t h e r & o r i g i n P r e f i x = F . . R P . . " ; 
 f o r   ( v a r   n a m e   i n   n u m A d s M a p )   { 
 v a r   m a p D a t a   =   i m p r e s s i o n M a p D a t a [ n a m e ] ; 
 v a r   r a i l A r g s   =   m a p D a t a   +   n u m A d s M a p [ n a m e ] ; 
 b a s e U R L   + =   r a i l A r g s ; 
 } 
 / /   H i d e   t o p   a d s   i f   w e   d i d n ' t   g e t   o n e 
 i f   ( i m p r e s s i o n M a p D a t a [ " a b o v e R e s u l t s T e x t A d s " ]   = =   n u l l   | |   i m p r e s s i o n M a p D a t a [ " a b o v e R e s u l t s T e x t A d s " ]   = =   0 )   { 
 j q ( " # t o p A d C o n t a i n e r " ) . a d d C l a s s ( ' h i d e e m p t y f i r s t ' ) . h i d e ( ) ; 
 }   e l s e   { 
 j q ( " # t o p A d C o n t a i n e r " ) . r e m o v e C l a s s ( ' h i d e e m p t y f i r s t ' ) . s h o w ( ) ; 
 } 
 j q . g e t ( b a s e U R L ) ; 
 } 
 v a r   m y A d C o u n t C a l l b a c k   =   g o o g l e T e x t A d C o u n t C a l l b a c k ; 
 v a r   t e x t A d s P a g e O p t i o n s   =   { 
 ' a d s R e s p o n s e C a l l b a c k ' :   m y A d C o u n t C a l l b a c k , 
 ' l i n k T a r g e t '   :   ' _ b l a n k ' , 
 ' a d p a g e ' :   1 ,   ' s e l l e r R a t i n g s ' :   t r u e ,   ' l o c a t i o n ' :   t r u e ,   ' q u e r y ' :   ' B e i j i n g   c h e a p   f l i g h t s ' ,   ' a d t e s t ' :   ' o f f ' ,   ' r i g h t H a n d A t t r i b u t i o n ' :   t r u e ,   ' f o n t S i z e D o m a i n L i n k ' :   1 1 ,   ' h l ' :   ' e n ' ,   ' o e ' :   ' u t f 8 ' ,   ' a d s a f e ' :   ' h i g h ' ,   ' c l i c k a b l e B a c k g r o u n d s ' :   t r u e ,   ' a t t r i b u t i o n T e x t ' :   ' a d s ' ,   ' p u b I d ' :   ' k a y a k ' ,   ' i e ' :   ' u t f 8 ' ,   ' q u e r y C o n t e x t ' :   ' c h e a p   f l i g h t s ' ,   ' c h a n n e l ' :   ' F R s l t ' ,   ' r o l l o v e r L i n k U n d e r l i n e ' :   t r u e   } ; 
 i f   ( a d S e n s e R a i l C o n f i g O p t i o n s . l e n g t h   = =   3 )   { 
 _ g o o g C s a ( ' a d s ' ,   t e x t A d s P a g e O p t i o n s ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 0 ] ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 1 ] ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 2 ] ) ; 
 }   e l s e   i f   ( a d S e n s e R a i l C o n f i g O p t i o n s . l e n g t h   = =   2 )   { 
 _ g o o g C s a ( ' a d s ' ,   t e x t A d s P a g e O p t i o n s ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 0 ] ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 1 ] ) ; 
 }   e l s e   i f   ( a d S e n s e R a i l C o n f i g O p t i o n s . l e n g t h   = =   1 )   { 
 _ g o o g C s a ( ' a d s ' ,   t e x t A d s P a g e O p t i o n s ,   a d S e n s e R a i l C o n f i g O p t i o n s [ 0 ] ) ; 
 } 
 l o a d D i s p l a y A d s ( ) ; 
 }   ( ) ) ; 
 < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t " > 
 v a r   v e r s a T a g   =   { 
 i d :   " 1 7 7 1 " , 
 s y n c :   0 , 
 d i s p T y p e :   " j s " , 
 p t c l :   " H T T P " , 
 b s U r l :   " b s . s e r v i n g - s y s . c o m / B u r s t i n g P i p e " , 
 a c t i v i t y P a r a m s :   { 
 / / P r e d e f i n e d   p a r a m e t e r s : 
 " S e s s i o n " : " l H - 0 r E h m H V a h 4 n f l o 0 l m S H E 6 1 y g " , 
 / / C u s t o m   p a r a m e t e r s : 
 " P r o d u c t   T y p e " : " f l i g h t " , 
 " P a g e   T y p e " : " r e s u l t s " , 
 " O r i g i n   I D " : " 1 6 0 7 8 " , 
 " R e f e r r i n g   s i t e   d o m a i n   l o c a l e " : " k a y a k . c o m " , 
 " D e s t i n a t i o n _ 3   l e t t e r   c o d e " : " P E K " , 
 " O r i g i n _ 3   l e t t e r   c o d e " : " L A X " , 
 " D e s t i n a t i o n   S t a t e _ C o u n t r y " : " B e i j i n g , C h i n a " , 
 " O r i g i n   S t a t e _ C o u n t r y " : " C a l i f o r n i a , U n i t e d   S t a t e s " , 
 " D e s t i n a t i o n   C i t y _ C i t y   n a m e " : " B e i j i n g " , 
 " O r i g i n   C i t y _ C i t y   n a m e " : " L o s   A n g e l e s " , 
 " K a y a k   T r a c k i n g   C o o k i e " : " l c O Z 1 J O i U n 8 2 9 W I E g k 4 v d - A H L l E " , 
 " A i r l i n e   I D " : " " , 
 " H o t e l   I D " : " " , 
 " T r a v e l   D a t e s _ E n d   D a t e " : " 2 0 1 5 0 1 1 2 " , 
 " T r a v e l   D a t e s _ S t a r t   D a t e " : " 2 0 1 4 1 2 2 4 " , 
 " D e s t i n a t i o n   I D " : " 3 2 8 6 " 
 } , 
 r e t a r g e t P a r a m s :   { } , 
 d y n a m i c R e t a r g e t P a r a m s :   { } , 
 c o n d i t i o n a l P a r a m s :   { } 
 } ; 
 j q ( f u n c t i o n ( )   { 
 s e t T i m e o u t ( f u n c t i o n   ( )   { 
 v a r   o l d W r i t e   =   d o c u m e n t . w r i t e ; 
 d o c u m e n t . w r i t e   =   f u n c t i o n ( n o d e )   { 
 j q ( " b o d y " ) . a p p e n d ( n o d e ) 
 } 
 j q . g e t S c r i p t ( " h t t p : / / d s . s e r v i n g - s y s . c o m / S e m i C a c h e d S c r i p t s / e b O n e T a g . j s " ,   f u n c t i o n ( )   { 
 s e t T i m e o u t ( f u n c t i o n ( ) { d o c u m e n t . w r i t e   =   o l d W r i t e } ,   1 0 0 ) 
 } ) 
 } ,   2 0 0 0 ) 
 } ) ; 
 < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t " > 
 / *   < ! [ C D A T A [   * / 
 v a r   g o o g l e _ c o n v e r s i o n _ i d   =   9 9 4 3 9 8 7 4 2 ; 
 v a r   g o o g l e _ c u s t o m _ p a r a m s   =   n u l l ; 
 v a r   g o o g l e _ r e m a r k e t i n g _ o n l y   =   t r u e ; 
 / *   ] ] >   * / 
 < / s c r i p t > 
 < s c r i p t   t y p e = " t e x t / j a v a s c r i p t "   s r c = " / / w w w . g o o g l e a d s e r v i c e s . c o m / p a g e a d / c o n v e r s i o n . j s " > 
 < / s c r i p t > 
 < n o s c r i p t > 
 < i m g   h e i g h t = " 1 "   w i d t h = " 1 "   s t y l e = " d i s p l a y : n o n e ; "   a l t = " " 
 s r c = " / / g o o g l e a d s . g . d o u b l e c l i c k . n e t / p a g e a d / v i e w t h r o u g h c o n v e r s i o n / 9 9 4 3 9 8 7 4 2 / ? v a l u e = 0 & g u i d = O N & s c r i p t = 0 " / > 
 < / n o s c r i p t > 
 < s c r i p t > 
 R 9 . A n a l y t i c s . a p i . t r a c k E C o m m e r c e ( { 
 " i d " :   " J l A C B _ r Q C 4 - 1 4 1 3 0 6 6 9 3 5 8 4 5 " , 
 " i t e m s " :   [ 
 { 
 " p r o d u c t " :   " f l i g h t   s e a r c h " 
 } 
 ] 
 } ) ; 
 < / s c r i p t > 
 < / d i v > 
 < s c r i p t > ( f u n c t i o n ( )   { 
 v a r   _ f b q   =   w i n d o w . _ f b q   | |   ( w i n d o w . _ f b q   =   [ ] ) ; 
 i f   ( ! _ f b q . l o a d e d )   { 
 v a r   f b d s   =   d o c u m e n t . c r e a t e E l e m e n t ( ' s c r i p t ' ) ; 
 f b d s . a s y n c   =   t r u e ; 
 f b d s . s r c   =   ' / / c o n n e c t . f a c e b o o k . n e t / e n _ U S / f b d s . j s ' ; 
 v a r   s   =   d o c u m e n t . g e t E l e m e n t s B y T a g N a m e ( ' s c r i p t ' ) [ 0 ] ; 
 s . p a r e n t N o d e . i n s e r t B e f o r e ( f b d s ,   s ) ; 
 _ f b q . l o a d e d   =   t r u e ; 
 } 
 } ) ( ) ; 
 w i n d o w . _ f b q   =   w i n d o w . _ f b q   | |   [ ] ; 
 w i n d o w . _ f b q . p u s h ( [ ' t r a c k ' ,   " 6 0 1 5 8 6 4 7 0 7 3 6 4 " ,   { ' v a l u e ' : ' 0 . 0 0 ' , ' c u r r e n c y ' : ' U S D ' } ] ) ; 
 < / s c r i p t > 
 < n o s c r i p t > < i m g   h e i g h t = " 1 "   w i d t h = " 1 "   a l t = " "   s t y l e = " d i s p l a y : n o n e "   s r c = " h t t p s : / / w w w . f a c e b o o k . c o m / t r ? e v = 6 0 1 5 8 6 4 7 0 7 3 6 4 & a m p ; c d [ v a l u e ] = 0 . 0 0 & a m p ; c d [ c u r r e n c y ] = U S D & a m p ; n o s c r i p t = 1 "   / > < / n o s c r i p t > 
 < / d i v > 
 < d i v   i d = " f t "   c l a s s = " f t   f a d e a b l e   
 " > 
 < d i v   i d = " c o m m o n f o o t e r " > 
 < s p a n   i d = " f o o t e r M e n u " > 
 < a   i d = " a b o u t u s - l i n k "   c l a s s = " c o r e "   h r e f = " / a b o u t " > A b o u t < / a > 
 < s p a n   c l a s s = " d i v i d e r " > & n b s p ; & b u l l ; & n b s p ; < / s p a n > 
 < a   i d = " p r i v a c y - l i n k "   c l a s s = " c o r e "   h r e f = " / p r i v a c y " > P r i v a c y < / a > 
 < s p a n   c l a s s = " d i v i d e r " > & n b s p ; & b u l l ; & n b s p ; < / s p a n > 
 < a   i d = " h e l p - l i n k "   c l a s s = " c o r e "   h r e f = " / h e l p "   t a r g e t = " _ b l a n k " > H e l p < / a > 
 < s p a n   c l a s s = " c o p y r i g h t " > 
 & c o p y ; 2 0 1 4   K A Y A K . c o m 
 < / s p a n > 
 < / s p a n > 
 < / d i v > 
 < / d i v > 
 < / b o d y > 
 < ! - -   C o p y r i g h t   2 0 0 4 - 2 0 1 4   K a y a k   S o f t w a r e   C o r p ,   A l l   R i g h t s   R e s e r v e d .   - - > 
 < / h t m l > 
� 'furlfile:///Users/Nan/Desktop/test.html� ��� � s o u r c e ( ' / U s e r s / N a n / D r o p b o x / M a c h i n e L e a r n i n g / f l i g h t _ p r i c e _ f u n c . R ' )�  �  �  �  �  �  �  �  �  �  �  �  �   ascr  ��ޭ