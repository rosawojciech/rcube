nawiasy.parse <- function(tekst,st = 1) {
  P <- list()
  spl <- strsplit(tekst,"")[[1]]
  licz <- 0
  for(i in st:length(spl))
  {
    if(spl[i]=='(')
    {
      licz <- licz +1
      if(is.null(P$p)) P$p <- i
    }
    if(spl[i]==')')
    {
      if(!is.null(P$p)) licz <- licz - 1
      if(licz == 0) if(is.null(P$k)) P$k <- i
    }
  }
  t <- TRUE
  sz <- P$k+1
  if(spl[sz]==' ') sz <- sz +1
  if(spl[sz]=='x') {
    sz <- sz +1
    P$arg <- 'x'}

  for(i in sz:length(spl))
    if(t){

      if(is.na(as.integer2(spl[i])))
      {t<- FALSE
      }
      else {P$arg <- paste0(P$arg,spl[i])
      P$kk <- i}
    }
  P$arg <- strsplit(P$arg,"")[[1]]
  P$typ <- "o"
  if(P$arg[1]=='x') P$typ <- "x"
  return(P)
}
rotacje.parse <- function(tekst) {
  rot <- gregexpr(pattern = "[(][a-zA-Z0-9]*[)][0-9]",tekst)[[1]]
  if(sum(rot)>0){
    for(i in rot)
    {
      rot <- gregexpr(pattern = "[(][a-zA-Z0-9]*[)][0-9]",tekst)[[1]]
      P <- nawiasy.parse(tekst,st = rot[1])
      tmp1 <- substr(tekst,1,P$p-1)
      tmp2 <- paste0(strsplit(substr(tekst,P$p+1,P$k-1),"")[[1]],P$arg,collapse = "")
      tmp3 <- substr(tekst,P$kk+1,100000)
      tekst <- paste0(tmp1,tmp2,tmp3)
    }}
  return(tekst)
}
kierunki.parse <- function(k) {

  k2 <- unlist(strsplit(k,split=""))
  k3 <- unlist(lapply(k2,function(i) as.integer2(i)))
  k3[is.na(k3)]<- 0
  k2 <- as.data.frame(k2)
  k2$O <- 0
  k5 <- which(k3>0)
  for(i in k5) {k2[i-1,'O'] <- k3[i]-1}
  k6 <- which(k3<2)
  k7 <- k2[k6,]
  k7 <- k7[k7$k2!="1",]
  ile <- sum(k7[,'k2']==':')
  i <- 1
  il <-0
  while(il<ile) {
    if(k7[i,'k2']==':'){
      l <- length(k7[,1])
      if(i < l) tmp <- k7[(i+1):length(k7[,'k2']),]
      else tmp <- data.frame()
      tmp2 <- k7[1:(i-2),]
      tmp3 <- as.data.frame(k7[i-1,'O']:k7[i,'O'])
      tmp3['k2'] <- as.character(k7[i-1,'k2'])
      colnames(tmp3)<-colnames(k7)[c(2,1)]
      tmp3 <- tmp3[c(2,1)]
      k7 <- rbind(tmp2,tmp3,tmp)
      if(i == 2) k7 <- k7[-1,]
      il <- il+1
    }
    i <- i+1
  }
  return(k7)
}
kostka.obrot <- function(kostka,kierunki,razy) {
  if(kierunki!='') {
    N <- sqrt(length(kostka)/3)/2
    N11 <- N+1
    N2 <- 2*N
    N21 <- 2*N+1
    N3 <- 3*N
    N31 <- 3*N+1
    N4 <- 4*N

    kierunki <- rotacje.parse(kierunki)
    if(sum(strsplit(kierunki,"")[[1]]=='x')>0)
    {
      P <- nawiasy.parse(kierunki)
      for(r in 1:razy){
        kostka <- kostka.obrot(kostka,substr(kierunki,1,P$p-1),1)
        kostka <- kostka.obrot(kostka,substr(kierunki,P$p+1,P$k-1),as.integer(paste0(P$arg[-1],collapse="")))
        kostka <- kostka.obrot(kostka,substr(kierunki,P$kk+1,100000),1)
      }
    }
    else{
      kp <- kierunki.parse(kierunki)
      kierunki <- kp[,1]
      obroty <- kp[,2]


      for(r in 1:razy)
      {i <- 1
      for(kierunek in kierunki){
        o <- obroty[i]
        i <- i+1
        if(kierunek=='d'){
          kostka[N2-o,] =c(kostka[N2-o,N11:N4],kostka[N2-o,1:N])
          if(o==0) {sciana <- kostka[N21:N3,N11:N2]
          kostka[N21:N3,N11:N2] <- t(sciana[,N:1])}
        }
        if(kierunek=='D'){
          kostka[N2-o,] =c(kostka[N2-o,N31:N4],kostka[N2-o,1:N3])
          if(o==0) {sciana <- kostka[N21:N3,N11:N2]
          kostka[N21:N3,N11:N2] <- t(sciana[N:1,])}
        }
        if(kierunek=='u'){
          kostka[N11+o,] =c(kostka[N11+o,N31:N4],kostka[N11+o,1:N3])
          if(o==0) {sciana <- kostka[1:N,N11:N2]
          kostka[1:N,N11:N2] <- t(sciana[,N:1])}
        }
        if(kierunek=='U'){
          kostka[N11+o,] =c(kostka[N11+o,N11:N4],kostka[N11+o,1:N])
          if(o==0) {sciana <- kostka[1:N,N11:N2]
          kostka[1:N,N11:N2] <- t(sciana[N:1,])}
        }
        if(kierunek=='l'){
          tmp <- kostka[1:N,N11+o]
          kostka[,N11+o] =c(kostka[N11:N3,N11+o],kostka[N2:N11,N4-o])
          kostka[N2:N11,N4-o] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,1:N]
          kostka[N11:N2,1:N] <- t(sciana[,N:1])}
        }
        if(kierunek=='L'){
          tmp <- kostka[N21:N3,N11+o]
          kostka[,N11+o] =c(kostka[N2:N11,N4-o],kostka[1:N2,N11+o])
          kostka[N2:N11,N4-o] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,1:N]
          kostka[N11:N2,1:N] <- t(sciana[N:1,])}
        }
        if(kierunek=='r'){
          tmp <- kostka[N21:N3,N2-o]
          kostka[,N2-o] =c(kostka[N2:N11,N31+o],kostka[1:N2,N2-o])
          kostka[N2:N11,N31+o] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,N21:N3]
          kostka[N11:N2,N21:N3] <- t(sciana[,N:1])}
        }
        if(kierunek=='R'){
          tmp <- kostka[1:N,N2-o]
          kostka[,N2-o] =c(kostka[N11:N3,N2-o],kostka[N2:N11,N31+o])
          kostka[N2:N11,N31+o] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,N21:N3]
          kostka[N11:N2,N21:N3] <- t(sciana[N:1,])}
        }
        if(kierunek=='f'){
          tmp <- kostka[N21+o,N11:N2]
          kostka[N21+o,N11:N2] <- kostka[N11:N2,N-o]
          kostka[N2:N11,N-o] <- kostka[N-o,N11:N2]
          kostka[N-o,N11:N2] <- kostka[N11:N2,N21+o]
          kostka[N2:N11,N21+o] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,N11:N2]
          kostka[N11:N2,N11:N2] <- t(sciana[,N:1])}
        }
        if(kierunek=='F'){
          kostka[N2:N11,N21+o] -> tmp
          kostka[N-o,N11:N2] -> kostka[N11:N2,N21+o]
          kostka[N2:N11,N-o] -> kostka[N-o,N11:N2]
          kostka[N21+o,N11:N2] -> kostka[N11:N2,N-o]
          tmp -> kostka[N21+o,N11:N2]
          if(o==0) {sciana <- kostka[N11:N2,N11:N2]
          kostka[N11:N2,N11:N2] <- t(sciana[N:1,])}
        }
        if(kierunek=='b'){
          tmp <- kostka[N11:N2,1+o]
          kostka[N11:N2,1+o] <- kostka[N3-o,N11:N2]
          kostka[N3-o,N2:N11] <- kostka[N11:N2,N3-o]
          kostka[N11:N2,N3-o] <- kostka[1+o,N11:N2]
          kostka[1+o,N2:N11] <- tmp
          if(o==0) {sciana <- kostka[N11:N2,N31:N4]
          kostka[N11:N2,N31:N4] <- t(sciana[,N:1])}
        }
        if(kierunek=='B'){
          kostka[1+o,N2:N11] -> tmp
          kostka[N11:N2,N3-o] -> kostka[1+o,N11:N2]
          kostka[N3-o,N2:N11] -> kostka[N11:N2,N3-o]
          kostka[N11:N2,1+o] -> kostka[N3-o,N11:N2]
          tmp -> kostka[N11:N2,1+o]
          if(o==0) {sciana <- kostka[N11:N2,N31:N4]
          kostka[N11:N2,N31:N4] <- t(sciana[N:1,])}
        }
        if(kierunek=='O'){
          sciana <- kostka[N11:N2,1:N]
          kostka[N11:N2,1:N3] <- kostka[N11:N2,N11:N4]
          kostka[N11:N2,N31:N4] <- sciana
          sciana <- kostka[1:N,N11:N2]
          kostka[1:N,N11:N2] <- t(sciana[N:1,])
          sciana <- kostka[N21:N3,N11:N2]
          kostka[N21:N3,N11:N2] <- t(sciana[,N:1])
        }
        if(kierunek=='o'){
          sciana <- kostka[N11:N2,N31:N4]
          kostka[N11:N2,N11:N4] <- kostka[N11:N2,1:N3]
          kostka[N11:N2,1:N] <- sciana
          sciana <- kostka[1:N,N11:N2]
          kostka[1:N,N11:N2] <- t(sciana[,N:1])
          sciana <- kostka[N21:N3,N11:N2]
          kostka[N21:N3,N11:N2] <- t(sciana[N:1,])
        }
        if(kierunek=='P'){
          sciana <- kostka[1:N,N11:N2]
          kostka[,N11:N2] <- rbind(kostka[N11:N3,N11:N2],kostka[N2:N11,N4:N31])
          kostka[N2:N11,N4:N31] <- sciana
          sciana <- kostka[N11:N2,N21:N3]
          kostka[N11:N2,N21:N3] <- t(sciana[N:1,])
          sciana <- kostka[N11:N2,1:N]
          kostka[N11:N2,1:N] <- t(sciana[,N:1])
        }
        if(kierunek=='p'){
          sciana <- kostka[N21:N3,N11:N2]
          kostka[,N11:N2] <- rbind(kostka[N2:N11,N4:N31],kostka[1:N2,N11:N2])
          kostka[N2:N11,N4:N31] <- sciana
          sciana <- kostka[N11:N2,N21:N3]
          kostka[N11:N2,N21:N3] <- t(sciana[,N:1])
          sciana <- kostka[N11:N2,1:N]
          kostka[N11:N2,1:N] <- t(sciana[N:1,])
        }
      }
      }
    }    }
  return(kostka)
}
as.integer2 <- function(arg){
  res <- NA
  if(check.integer(arg)) res <- as.integer(arg)
  return(res)
}
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}
draw_facelet <- function(x,y,col){
  xs <- c(-1,0,1,0) + x
  ys <- c(0,-1,0,1) + y
  polygon(xs, ys, col = col, lty =1 , lwd = 1, border = "black")
}
draw_facelet2 <- function(x,y,col){
  xs <- c(0,1,1,0)  + x
  ys <- c(-1,0,-2,-3) + y
  polygon(xs, ys, col = col, border = "black")
}
draw_facelet3 <- function(x,y,col){
  xs <- c(-1,0,0,-1) + x
  ys <- c(0,-1,-3,-2)  + y
  polygon(xs, ys, col = col, lty =1 , lwd = 1, border = "black")
}

plot3Dtop <- function(cube) {
  N <- cube$size
  plot(NULL,xlim = c(-N,N), ylim = c(-(2*N+1),2*N-1),asp = 0.6,axes = FALSE,xlab = "",ylab = "")

  k <- 0
  top <- cube$cube[N:1,(N+1):(2*N)]
  for(i in 0:(N-1)) for(j in 0:(N-1))
  { k <- k +1
  draw_facelet(i-j,i+j,cube$scheme[c(top)][k])
  }
  k <- 0
  front <- cube$cube[(2*N):(N+1),(N+1):(2*N)]
  for(i in 0:(N-1)) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet2(i,i+2*j,cube$scheme[c(front)][k])}
  k <- 0
  left <- cube$cube[(2*N):((N+1)),1:N]
  for(i in (-N+1):0) for(j in (-N+1):0)
  { k <- k+1
  draw_facelet3(i,2*j-i,cube$scheme[c(left)][k])
  }
}
plot3Dbottom <- function(cube) {
  N <- cube$size
  plot(NULL,xlim = c(-N,N), ylim = c(-(2*N+1),2*N-1),asp = 0.6,axes = FALSE,xlab = "",ylab = "")

  k <- 0
  bottom <- t(cube$cube[(2*N+1):(3*N),(N+1):(2*N)])
  for(i in 0:(N-1)) for(j in 0:(N-1))
  { k <- k +1
  draw_facelet(i-j,i+j-2*N,cube$scheme[c(bottom)][k])
  }
  k <- 0
  right <- cube$cube[(2*N):(N+1),(2*N+1):(3*N)]
  for(i in 0:(N-1)) for(j in (-N+1):0) {
    k <- k+1
    draw_facelet2(i-N,i+2*j+N,cube$scheme[c(right)][k])}
  k <- 0
  back <- cube$cube[(2*N):((N+1)),(3*N+1):(4*N)]
  for(i in (-N+1):0) for(j in (-N+1):0)
  { k <- k+1
  draw_facelet3(i+N,2*j-i+N,cube$scheme[c(back)][k])
  }
}
