
    section .data
 
        coeff_i: dq 0
        func_array:  dq 0;array for the real part
        iArray:  dq 0;array for the im part
        derivArray: dq 0

        ans: dq 0 , 0
        order_string: dd "%d",0
        epsilon_format: db "epsilon = %lf",0
        order_format: db 10,"order = %d",0
        coeff_format: db 10,"%*s %d %*s %lf %lf",0
        inital_format: db 10,"initial = %lf %lf",0
        complex_print: db "Real: %.17lf  Im: %.17lf",10,0
        print_t: db "",0


    section .bss
        buffer:  resq 1
        order_buf: resb 100
        arg1: resq 1
        arg2: resq 1
        epsilon: resq 1
        order:resq 1
        deri_order:resq 1
        imbuf: resq 2
        deri_buff: resq 2
        initial: resq 2
        tempIm: resq 2
        breg: resb 4
        reg0: resq 1
        reg1: resq 1
        reg2: resq 1
        reg3: resq 1
        root: resq 2
        
    
        
    section .text
  
        global main
        extern malloc
        extern calloc
        extern free
        extern scanf
        extern printf
        
%macro loadInput 2    ;macro for loading input
                      ;argument one is the format to scan the desired input
                      ;argument two is the memory location(qword) for the desired input
        jmp %%Lp
        
        %%M:
            db %1
            
        %%Lp:
            mov rdi, %%M        ;putting macro first arg as first arg to function
            mov rsi, buffer     ; putting buffer as second arguement
            mov rax,0
            call scanf          ;scanning input
            mov rdx,[buffer]    ;putting retrived input into rbx
            mov [%2],rdx        ;putting retrived input into memory location pointed by the macro second arg
        
            
%endmacro


%macro adds 2 
    ;num1 num2
    fld qword [%1+8]
    fld qword [%2+8]
    faddp
    fld qword [%1]
    fld qword [%2]
    faddp
    
%endmacro

%macro mult 2 ;  Im n1 , Im n2

    fld qword [%1]
    fld qword [%2+8]
    fmulp               ; n1.real*n2.i
    fld qword [%2]
    fld qword [%1+8]
    fmulp               ;st0 :n2.real*n1.i ,st1 : n1.real*n2.i
    faddp               ;st0: n1.real*n2.i + n2.real*n1.i
    fld qword [%1]
    fld qword [%2]
    fmulp
    fld qword [%1+8]
    fld qword [%2+8]
    fmulp
    fchs
    faddp               ;st0: n1.real*n2.real +(-1)*n1.i*n2.i
                        ;st1: n1.real*n2.i + n2.real*n1.i
    
%endmacro

%macro powa 2; arg1 = num, arg2= power
    mov r9,%2
    cmp r9,0
    jne %%nz
       fldz
       fld1
       jmp %%powa_end
    %%nz:
      fld qword [%1]
      fstp qword [imbuf]
      fld qword [%1+8]
      fstp qword [imbuf+8]
      
      cmp r9,1
      je %%power_one
      mov r9,1
     %%powerLoop:
        mult %1 ,imbuf
        fstp qword [imbuf]
        fstp qword [imbuf+8]
        inc r9
        cmp r9,%2
        jl %%powerLoop
        %%power_one:
        fld qword [imbuf+8];push the ans back to the stack
        fld qword [imbuf]
        
      ;  mov qword [imbuf],0;zero the temp variable
     ;   mov qword [imbuf+8],0
        %%powa_end:  
       
      
%endmacro

%macro derive 0
   push r15
   push r10
    mov r15,qword [order]
    cmp r15,0
    jne %%nz
     fldz
     fldz
     jmp %%endit
    %%nz:
     mov r15,0
        deri_loop:
            mov qword [imbuf],r15
            mov r10,1
            add qword [imbuf],r10
            fild qword [imbuf]
            ;mov qword [deri_buff],r10
            fstp qword [deri_buff]
            
            ;add qword [imbuf],r10
            fldz
            fstp qword [deri_buff+8]
            mov rax,16          ; move to the req number
            mov r10,r15
            inc r10
            mul r10
            mov rsi,qword [func_array]
            add rsi,rax;move the array pointer to the wanted numba
            mult rsi,deri_buff
            mov rax,16
            mul r15
            mov rsi,qword [derivArray]
            add rsi,rax
            fstp qword [rsi]
            fstp qword [rsi+8]
            inc r15
            cmp r15,qword [order]
            jl deri_loop
            
         ;   func deri_order, derivArray , root
            
        ;use func with order-1, derivArray;;--;;--;;;;
       %%endit:
       pop r10;
       pop r15
        
%endmacro

%macro load_coeff 1
    jmp %%L
    
    %%M:
        db %1
    %%L:
        mov rdi, %%M
        mov rsi,buffer
        mov rdx,arg1
        mov rcx,arg2
        mov rax,0
        call scanf
        ;------load into the array
        mov rax,16
        mov rdi,qword [buffer]
        mul rdi
        mov rsi,qword [func_array]      ;move the function array
        add rsi,rax                     ;to the req position 
        fld     qword [arg1] ;--------------------------------------
        fstp    qword [rsi]  ; move the loaded coeff into the memory
        fld     qword[arg2]  ;
        fstp    qword [rsi+8];---------------------------------------
      
        
%endmacro


%macro print_Complex_fromStack 1 ;in the this macro we expect 
                                 ; that the number to be printed is in the
        jmp %%L
    
    %%M:
        db %1
    %%L:
       mov rdi,%%M
       fstp qword [arg1]
       movsd xmm0,[arg1]
       fstp qword [arg2]
       movsd xmm1,[arg2]
       mov rax,2
       call printf
       
%endmacro

 %macro enter 0
	push	rbp
	mov		rbp,rsp
%endmacro

%macro leave 0
	mov     rsp, rbp
    pop     rbp
    ret 		; Back to caller
%endmacro

%macro subIm 2 		;(Im* Im*)
        finit
	fld qword [%1+8]
        fld qword [%2+8]
        fsubp
        fld qword [%1]
        fld qword [%2]
        fsubp
	
%endmacro	

%macro absIm 1		;(Im*)
       ; finit
	fld 	qword 	[%1]	;put real in stack
	fmul 	st0				;square
	fld 	qword 	[%1+8]	;put imagi in stack
	fmul 	st0			;square
	faddp			;st0 = abs^2
	fsqrt				;abs in st0
%endmacro

%macro divIm 2		;(Im* Im*)
        finit
	movsd xmm0,    qword [%2]	;xmm0 = n2.real
	fld	       qword [%2+8]     ;push n2.i
	fchs                            ;top = (-1)*n2.i
	fstp    qword [reg0]            ;pop(reg0)
	movsd xmm1, qword [reg0]	;xmm1 = (-1)*n2.i
	movsd qword [imbuf], xmm0       ;imbuf = zamud
	movsd qword [imbuf+8],xmm1      ;zamud.i
	mult %1, imbuf			;ans = n1*zamud
	fstp    qword [reg0]            ;reg0 = ans.r
	fstp    qword [reg1]            ;reg1 = ans.i
	mult %2, imbuf			;zamud = 2*zamud
	fstp    qword [reg2]            ;reg2 = zamud.r
	movsd xmm0, qword [reg1]        ;xmm0 = ans.i
	movsd xmm1, qword [reg2]        ;xmm1 = zamud.r
	divsd xmm0, xmm1                ;xmm0 = ans.i/zamud.r
	movsd qword [reg1], xmm0        ;reg1 = ans.i/zamud.r
	movsd xmm0, qword [reg0]        ;xmm0 = ans.r
	movsd xmm1, qword [reg2]        ;xmm1 = zamud.r
	divsd xmm0, xmm1                ;xmm0 = ans.r/zamud.r
	movsd qword [reg0], xmm0        ;reg0 = new ans.r
        fld qword [reg1]                ;push new ans.i                                
	fld qword [reg0]		;push ans.real
%endmacro 

%macro func 3 ;order,coeffs[]
    finit
    push rbx

    fld qword [%2+8]         ;push coeffs[0].i
    fld qword [%2]           ;push coeffs[0].r
    fstp qword [ans]
    fstp qword [ans+8]
  ;  fld qword [%2+8]         ;push coeffs[0].i
 ;   fld qword [%2]
    mov r10, qword [%1]    ;r10 = order 
    cmp r10, 0                  ;if order == 0
    mov rdi,print_t;
     mov rax,0; i dont know why but it works...
    call printf
    je %%eend                    ;return coeffs[0]   

   mov rbx,0
   mov qword [ans],rbx
   mov qword [ans+8],rbx

%%loop:

    powa %3,rbx          ;power(z,j)
    fstp qword [tempIm]           ;temp.r = power(z,j).r
    fstp qword [tempIm+8]         ;temp.i = power(z,j).i
    
    mov rax, 16                   ;rax = 16
    mul rbx                       ;rax = 16*j
    mov rsi, [%2]                ;rsi = func_array
    add rsi,rax                 ;rax = func_array + 16*j
    mult rsi, tempIm              ;mul(func_array[j],power(z,j))
    fstp qword [tempIm]           ;temp.r = mul(coeffs[j],power(z,j)).r
    fstp qword [tempIm+8]         ;temp.i = mul(coeffs[j],power(z,j)).i  
    adds ans, tempIm              ;add(ans,mul(coeffs[j],power(z,j)))
    fstp qword [ans]              ;ans.r
    fstp qword [ans+8]            ;ans.i
    inc rbx                       ;j++
    cmp rbx,qword [%1]
    jle %%loop
                    ;push ans.r
%%eend:
 fld qword [ans+8]               ;push ans.i
 fld qword [ans]

 pop rbx
%endmacro

%macro newton 0
   finit
    %%loop:
    func order, func_array ,root
    fstp qword [deri_buff]
    fstp qword [deri_buff+8]        ;imbuf = Func(order, coeffs, ans) (for later use)
    absIm deri_buff
    fstp qword [reg0]                 ;reg0 = absIm(Func(order, coeffs, ans)

    fld qword [reg0]
    fld qword [epsilon]
    fcomip
    fstp
    ja %%iend
    
    func deri_order,derivArray,root

    fstp qword [tempIm]
    fstp qword [tempIm+8]       ;tempIm = derive(order, coeffs, ans)

    divIm deri_buff, tempIm
    fstp qword [tempIm]
    fstp qword [tempIm+8]       ;tempIm = divi(Func(order, coeffs, ans) ,derive(order, coeffs, ans))
    subIm root, tempIm

    fstp qword [root]            ;
    fstp qword [root+8]          ;ans = ans - tempIm

   jmp %%loop
    %%iend:
  ;  finit
    fld qword [root+8]
    fld qword [root]
    
%endmacro


        main:
        push	rbp
	mov	rbp,   rsp 
	
	
    finit
       mov rdi,epsilon_format
       mov rsi,epsilon
       call scanf
       mov rdi,order_format
       mov rsi,order
       call scanf
     ;-----------CALLOCS------------;  
       mov rax,16
       mov rdi,qword [order]    ; malloc the array for the coeffs
       inc rdi                  ;
       mul rdi                  ;    
       mov rdi,rax
       call malloc   
       mov qword [func_array],rax
       mov rax,16                ;malloc the array for the deriv
       mov rdi,qword [order]     ; 
       mul rdi
       mov rdi,rax

       call malloc
       mov qword [derivArray],rax
      ;----------MALLOCS-----------;  
        mov r14,[order]
        inc r14
        .my_loop:
    
        load_coeff {10,"%*s %d %*s %lf %lf",0}

            dec r14
            cmp r14,  0
        jg .my_loop
        
        mov rdi,inital_format
        mov rsi,initial
        mov rdx,initial
        add rdx,8
      ;  mov rax,0
        call scanf
       ; fstp qword [deri_order]
        fld qword [initial+8]
        fld qword [initial]
        fstp qword [root]
        fstp qword [root+8]
        
        mov rdi, qword [order]
        dec rdi
        mov qword [deri_order],rdi
     
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
       
      derive
      newton
      print_Complex_fromStack{"root = %.17lf %.17lf",10,0}
    
      mov rdi,qword [func_array]
      call free
      mov rdi,qword [derivArray]
      call free
       
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        
      
        
        
        end:
        mov     rsp, rbp
        pop     rbp
        ret 

        
        