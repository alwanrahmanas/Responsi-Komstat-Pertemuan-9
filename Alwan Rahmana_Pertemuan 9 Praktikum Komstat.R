#no 1
n1<-25
xbar1<-44.5
s1<-3.6
alpha1= 0.01
#H0: mu= 43
#Ha: mu!= 43

#jika |z hitung| lebih dari |z alpha/2| maka tolak H0


z1<-(xbar1-43)/(s1/sqrt(n))
za21<-qnorm(0.995,0,1)
za21

if(z1>abs(za21)) {print("Tolak H0: Dengan tingkat signifikansi 1% cukup bukti untuk menyatakan bahwa rata-rata populasi != 43.")
}else{print("Gagal tolak H0: Dengan tingkat signifikansi 1% belum cukup bukti untuk menyatakan bahwa
         rata-rata populasi != 43.")} 


#no 2


#cek apakah kita dapat menyimpulkan bahwa mobil dengan ban radial lebih
#hemat bahan bakar daripada mobil ban biasa?
alpha2<-0.025
BB<-c(4.1,4.9,6.2,6.9,6.8,4.4,5.7,5.8,6.9,4.7,6,4.9)
BR<-c(4.2,4.7,6.6,7,6.7,4.5,5.7,6,7.4,4.9,6.1,5.2)

n<-12
xbarBB<-mean(BB,na.rm = TRUE)

xbarBR<-mean(BR,na.rm=TRUE)


#Uji-F
var.test(p$BB,p$BR)
#H0: Ration varians = 1
#Ha: Ratio varians != 1
#Tolak Ho: cukup bukti untuk menyatakan bahwwa ratio varians != 1



t.test(BB, BR, var.equal = FALSE,conf.level = 1-alpha2)
#H0: mu1-mu2= 0
#Ha: mu1-mu2!= 0

#varians populasi tidak diketahui dan tidak sama
#jika |t hitung| lebih dari |t alpha/2| maka tolak H0
# tolak H0: true difference in means is not equal to 0

#nomor 3

A<-c(73,43,47,53,58,47,52,38,61,56,56,34,55,65,75)
B<-c(51,41,43,41,47,32,24,43,53,52,57,44,57,40,68)

shapiro.test(A)
#H0: Data berdistribusi normal
#Ha: Data tidak berdistribusi normal

#A berdistribusi normal

shapiro.test(B)
#H0: Data berdistribusi normal
#Ha: Data tidak berdistribusi normal

#B berdistribusi normal

var.test(A,B)
#H0: Ration varians = 1
#Ha: Ratio varians != 1
#Tolak Ho: cukup bukti untuk menyatakan bahwwa ratio varians != 1

#cek kualitas cat berdasarkan rata-rata tingkat karat

t.test(A,B,var.equal=FALSE)
#H0: mu1-mu2= 0
#Ha: mu1-mu2!= 0
#varians populasi tidak diketahui dan tidak sama
#jika |t hitung| lebih dari |t alpha/2| maka tolak H0
# tolak H0: true difference in means is not equal to 0