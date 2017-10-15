
numletra :: Int -> String
numletra num
    | (num==0) = "cero"
	| num > 0 && num<16 = 
	let answers = ["uno", "dos", "tres", "cuatro", "cinco","seis","siete","ocho","nueve","dies","once","doce","trece","catorce","quince"]
	in answers!!(num-1)
	 | (num<20) = "dieci"++numletra(num-10)
     | (num==20) = "veinte"
     | (num<30) = "veinti "++numletra(num-20)
     |(or [num==30,num==40,num==50,num==60,num==70,num==80,num==90]) = 
     	let answers = ["treinta","cuarenta","cincuenta","sesenta","setenta","ochenta","noventa"]
     	in answers!!((num `div` 10)-3)
     | (num<100) = numletra((num `div` 10) * 10) ++ " y " ++ numletra(num `mod` 10)
     | (num==100) = "cien"
     | (num <200) = "ciento " ++ numletra(num-100)
     | (or [num==200,num==300,num==400,num==600,num==800])= numletra((num `div` 100)) ++ "cientos"
      
      |(or [num==500,num==700,num==900]) = 
     	let answers = ["quinientos","","setecientos","","novecientos"]
     	in answers!!((num `div` 100)-5)
     | (num<1000) = numletra((num `div` 100)*100)++ " " ++ numletra(num `mod` 100)
     | (num==1000)="mil"
     | (num<2000)= "mil " ++ numletra(num `mod` 1000)
     | (num<1000000) = numletra(num `div` 1000) ++ " mil " ++ numletra(num `mod` 1000)
     | (num==1000000)="un millon"



