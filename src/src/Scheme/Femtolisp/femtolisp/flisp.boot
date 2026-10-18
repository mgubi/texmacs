(%fl:1+ #4=#fn("6000r1|aw;" #() 1+) %fl:1-
	#5=#fn("6000r1|ax;" #() 1-) %fl:1arg-lambda? #6=#fn("7000r1|F16Z02|Mc0<16P02|NF16H02e1|31F16=02e2e1|31a42;" #(lambda
  %fl:cadr %fl:length=) 1arg-lambda?)
	%fl:<= #7=#fn("6000r2}|X640^;|D6>0|}X640];|}W;];" #() <=) %fl:>
	#8=#fn("6000r2}|X;" #() >) %fl:>= #9=#fn("7000r2e0}|42;" #(%fl:<=) >=)
	%fl:__init_globals #10=#fn("6000r0e0c1<17B02e0c2<17802e0c3<6>0c4k52c6k75;0c8k52c9k72e:k;2e<k=2e>k?;" #(*os-name*
  win32 win64 windows "\\" *directory-separator* "\x0d\n" *linefeed* "/" "\n"
  *stdout* *output-stream* *stdin* *input-stream* *stderr* *error-stream*) __init_globals)
	%fl:__script #11=#fn("6000r1c0|q1c1t;" #(#fn("6000r0e0~41;" #(%fl:load))
						 #fn("6000r1e0|312c1a41;" #(%fl:top-level-exception-handler
  #fn(exit)))) __script)
	%fl:__start #12=#fn("7000r1e0302|NF6G0|Nk12^k22e3e4|31315E0|k12]k22e5e6312e7302c8`41;" #(%fl:__init_globals
  *argv* *interactive* %fl:__script %fl:cadr %fl:princ *banner* %fl:repl #fn(exit)) __start)
	%fl:abs #13=#fn("6000r1|`X650|y;|;" #() abs) %fl:any
	#14=#fn("7000r2}F16D02|}M3117:02e0|}N42;" #(%fl:any) any)
	%fl:argc-error #15=#fn(";000r2e0c1|c2}}aW670c3540c445;" #(%fl:error "compile error: "
								  " expects "
								  " argument."
								  " arguments.") argc-error)
	%fl:array #fn(array) %fl:array?
	#16=#fn("7000r1|H17F02c0|31g5F16902g5Mc1<;" #(#fn(typeof) array) array?)
	%fl:assoc #17=#fn("7000r2}?640^;e0}31|>650}M;e1|}N42;" #(%fl:caar
								 %fl:assoc) assoc)
	%fl:assv #18=#fn("7000r2}?640^;e0}31|=650}M;e1|}N42;" #(%fl:caar
								%fl:assv) assv)
	%fl:bcode:cenv #19=#fn("6000r1|b3[;" #() bcode:cenv) %fl:bcode:code
	#20=#fn("6000r1|`[;" #() bcode:code) %fl:bcode:ctable #21=#fn("6000r1|a[;" #() bcode:ctable)
	%fl:bcode:indexfor #22=#fn(";000r2e0|31e1|31c2g6}326:0c3g6}42;c4g6}g7332g7|b2g7aw\\2;" #(%fl:bcode:ctable
  %fl:bcode:nconst #fn(has?) #fn(get) #fn(put!)) bcode:indexfor)
	%fl:bcode:nconst #23=#fn("6000r1|b2[;" #() bcode:nconst) %fl:bcode:sp
	#24=#fn("6000r1|b4[;" #() bcode:sp) %fl:bcode:stack #25=#fn("8000r2|b4[6=0|b4|b4[}w\\;c0;" #(#{#<unspecified>}#) bcode:stack)
	%fl:box-vars #26=#fn("9000r4c0c1g2g3|q3}42;" #(#fn(for-each)
						       #fn("9000r1e0|3116I02e1e2|31~3216:02e2|31\x7fX6@0e3i2c4e2|3143;c5;" #(%fl:vinfo:heap?
  %fl:>= %fl:vinfo:index %fl:emit box #{#<unspecified>}#))) box-vars)
	%fl:bq-bracket #27=#fn(";000r2|?6=0c0e1|}32L2;|Mc2‚R0}`W680c0|NK;c0c3c4e1|N}ax32L3L2;|Mc5‚Y0}`W6<0c6e7|31L2;c0c0c8e1e7|31}ax32L3L2;|Mc9‚U0}`W680e7|41;c0c0c:e1e7|31}ax32L3L2;c0e1|}32L2;" #(#.list
  %fl:bq-process unquote #.cons 'unquote unquote-splicing copy-list %fl:cadr 'unquote-splicing
  unquote-nsplicing 'unquote-nsplicing) bq-bracket)
	%fl:bq-bracket1 #28=#fn(":000r2|F16802|Mc0<6N0}`W680e1|41;c2c3e4|N}ax32L3;e4|}42;" #(unquote
  %fl:cadr #.cons 'unquote %fl:bq-process) bq-bracket1)
	%fl:bq-process #29=#fn("<000r2|C680c0|L2;|H6T0e1e2|31}32g6Mc3‚90c4g6NK;c5c4g6L3;|?640|;|Mc6‚E0c3c7e1e8|31}aw32L3;|Mc9‚Z0}`W16:02e:|b232680e8|41;c;c<e1|N}ax32L3;e=e>|327t0e?|31c@cA}q1|32g6…80c3g7K;g7N…A0c;g7Me1g6}32L3;cBcCg7Ke1g6}32L142;cDŒ6g6cE}g6q2OM2g6MŽ1|_42;" #(quote
  %fl:bq-process %fl:vector->list #.list #.vector #.apply quasiquote 'quasiquote
  %fl:cadr unquote %fl:length= #.cons 'unquote %fl:any %fl:splice-form?
  %fl:lastcdr #fn(map) #fn("7000r1e0|~42;" #(%fl:bq-bracket1))
  #fn(nconc) #fn(list*) #{#<unspecified>}# #fn("=000r2|…;0c0e1}31K;|F6n0|Mc2‚W0c0e3}~`W670|N5C0c4c5L2e6|N~ax32L232K;\x7fM|Ne7|M~32}K42;c0e1e6|~32}K31K;" #(nconc
  %fl:reverse! unquote %fl:nreconc #.list 'unquote %fl:bq-process
  %fl:bq-bracket))) bq-process)
	%fl:builtin->instruction #30=#fn("8000r1c0~|^43;" #(#fn(get)) #(#table(#.equal? equal?  #.* *  #.car car  #.apply apply  #.aref aref  #.- -  #.boolean? boolean?  #.builtin? builtin?  #.null? null?  #.eqv? eqv?  #.function? function?  #.bound? bound?  #.cdr cdr  #.list list  #.set-car! set-car!  #.cons cons  #.atom? atom?  #.set-cdr! set-cdr!  #.symbol? symbol?  #.eq? eq?  #.vector vector  #.not not  #.pair? pair?  #.number? number?  #.div0 div0  #.aset! aset!  #.+ +  #.= =  #.compare compare  #.vector? vector?  #./ /  #.< <  #.fixnum? fixnum?)))
	%fl:builtin-argc-ok? #31=#fn("8000r2c0e1|^33g6@17702}g6W16\\02}`W16@02|c2<17702|c3<@16A02}b2X16702|c4<@;" #(#fn(get)
  arg-counts #.- #./ #.apply) builtin-argc-ok?)
	%fl:byte #fn(byte) %fl:caaaar
	#32=#fn("5000r1|MMMM;" #() caaaar) %fl:caaadr #33=#fn("5000r1|„MM;" #() caaadr)
	%fl:caaar #34=#fn("5000r1|MMM;" #() caaar) %fl:caadar
	#35=#fn("5000r1|M„M;" #() caadar) %fl:caaddr #36=#fn("5000r1|N„M;" #() caaddr)
	%fl:caadr #37=#fn("5000r1|„M;" #() caadr) %fl:caar
	#38=#fn("5000r1|MM;" #() caar) %fl:cadaar #39=#fn("5000r1|MM„;" #() cadaar)
	%fl:cadadr #40=#fn("5000r1|„„;" #() cadadr) %fl:cadar
	#41=#fn("5000r1|M„;" #() cadar) %fl:caddar #42=#fn("5000r1|MN„;" #() caddar)
	%fl:cadddr #43=#fn("5000r1|NN„;" #() cadddr) %fl:caddr
	#3=#fn("5000r1|N„;" #() caddr) %fl:cadr #2=#fn("5000r1|„;" #() cadr)
	%fl:call-with-values #44=#fn("7000r2|30g6F16802~g6M<690}g6Nv2;}g641;" #() #(#1=(*values*)))
	%fl:capture-var! #45=#fn("<000r2e0|31e1}g6`33g717G02c2g631|b3c3g6}L132\\2;" #(%fl:bcode:cenv
  %fl:index-of #fn(length) #fn(append!)) capture-var!)
	%fl:cdaaar #46=#fn("5000r1|MMMN;" #() cdaaar) %fl:cdaadr
	#47=#fn("5000r1|„MN;" #() cdaadr) %fl:cdaar #48=#fn("5000r1|MMN;" #() cdaar)
	%fl:cdadar #49=#fn("5000r1|M„N;" #() cdadar) %fl:cdaddr
	#50=#fn("5000r1|N„N;" #() cdaddr) %fl:cdadr #51=#fn("5000r1|„N;" #() cdadr)
	%fl:cdar #52=#fn("5000r1|MN;" #() cdar) %fl:cddaar
	#53=#fn("5000r1|MMNN;" #() cddaar) %fl:cddadr #54=#fn("5000r1|„NN;" #() cddadr)
	%fl:cddar #55=#fn("5000r1|MNN;" #() cddar) %fl:cdddar
	#56=#fn("5000r1|MNNN;" #() cdddar) %fl:cddddr #57=#fn("5000r1|NNNN;" #() cddddr)
	%fl:cdddr #58=#fn("5000r1|NNN;" #() cdddr) %fl:cddr
	#59=#fn("5000r1|NN;" #() cddr) %fl:char? #60=#fn("6000r1c0|31c1<;" #(#fn(typeof)
  wchar) char?)
	%fl:closure? #61=#fn("6000r1|J16602|G@;" #() closure?) %fl:compare<=
	#62=#fn("?000r2c0c1}|L3^c0c2|L2c0c1|}L3]c3|}L3L4]L4L4;" #(if < number?
								  =) compare<=)
	%fl:compile #63=#fn("8000r1e0_e1|3142;" #(%fl:compile-f
						  %fl:lower-define) compile)
	%fl:compile-and #64=#fn(";000r4e0|}g2g3]c146;" #(%fl:compile-short-circuit
							 brf) compile-and)
	%fl:compile-app #65=#fn(":000r4e0g3}32g86=0e1|}g2g844;e2|}g2g344;" #(%fl:simple-compare
  %fl:compile-in %fl:compile-app*) compile-app)
	%fl:compile-app* #66=#fn("H000r4g3Mg8C16;02e0g8}32@6;0e1g831540g8g8C16Z02e0g8}32@16M02g9E16E02c2g93116:02c3g931G6;0c3g931540g8g:c4<17802g:c5<16<02e6g3b332@6M0e7|}g2g:c4‚70c8540c9g3NK44;e:g3Nc;326k0e7|}^g:342e<|a322e=|}g3N33e<|b/g;x322e>|g2670c?540c@g;43;g:G16K02eAg:cBg3N313216902eCg:31g:cD<16`02g9cD<16V02e0g:}32@16I02c3cD31eE>16;02e6g3b2326H0e7|}^eEg331342e>|cD42;g:F16N02g:McF<16C02eG|3116902eHg3316=0eI|}g2g344;g;7E0e7|}^g:342e<|a32540cJ2e=|}g3N33e<|g<y322g;7<0e<|b/32540cJ2g;6C0eK|}g2g3g:g;g<47;e>|g2670cL540cMg<43;" #(%fl:in-env?
  resolve-global #fn(constant?) #fn(top-level-value) #.< #.= %fl:length=
  %fl:compile-in nary< nary= %fl:length> 255 %fl:bcode:stack
  %fl:compile-arglist %fl:emit tcall.l call.l %fl:builtin-argc-ok?
  #fn(length) %fl:builtin->instruction cadr %fl:cadr lambda %fl:bcode:sp
  %fl:inlineable? %fl:compile-let #{#<unspecified>}# %fl:compile-builtin-call
  tcall call) compile-app*)
	%fl:compile-arglist #67=#fn("8000r3c0c1|}q2g2322c2g241;" #(#fn(for-each)
								   #fn("9000r1e0~\x7f^|342e1~a42;" #(%fl:compile-in
  %fl:bcode:stack)) #fn(length)) compile-arglist)
	%fl:compile-begin #68=#fn("9000r4g3?6?0e0|}g2e13044;g3N?6>0e0|}g2g3M44;e0|}^g3M342e2|c3322e4|}g2g3N44;" #(%fl:compile-in
  %fl:void %fl:emit pop %fl:compile-begin) compile-begin)
	%fl:compile-builtin-call #69=#fn("<000r7c0e1g4^33g;16=02e2g3Ng;32@6=0e3g5g;32540c42g5g<c5‚K0g6`W6:0e6|c742;e6|g5g643;g<c8‚[0g6`W6:0e6|c942;g6b2W6:0e6|c:42;e6|g5g643;g<c;‚j0g6`W6:0e3g5a42;g6aW6:0e6|c<42;g6b2W6:0e6|c=42;e6|g5g643;g<c>‚K0g6`W6:0e6|c?42;e6|g5g643;g<c@‚K0g6`W6:0e3g5a42;e6|g5g643;g<cA‚M0g6`W6<0e6|cBcC43;e6|g5g643;g<cD‚W0g6b2X6;0e3g5b242;e6|g2670cE540cDg643;e6|g542;" #(#fn(get)
  arg-counts %fl:length= %fl:argc-error #{#<unspecified>}# list %fl:emit
  loadnil + load0 add2 - neg sub2 * load1 / vector loadv #() apply tapply) compile-builtin-call)
	%fl:compile-f #70=#fn("8000r2e0c1|}q2c242;" #(%fl:call-with-values
						      #fn("7000r0e0~\x7f42;" #(%fl:compile-f-))
						      #fn("5000r2|;" #())) compile-f)
	%fl:compile-f- #71=#fn("Q000r2e030e1}31e2e1}3131e3}31e4c5e1}3132e2}31g;…70c6540g;g7?660`580c7g731g=c7g:31xe4e8g:32e9e:}31g9c;e1g:32Q4e<|g9g@33g:‡±0g?…L0e=g6c>g>g8…70g=550g=y345r0e?g6e@c;cAc;cBg?32eCc7g?31313331322e=g6cDg>c7g?31g8…70g=550g=y352eEg6gAM`g>342eFg6|g:g9g>g@36540cG2eHg=cI326I0e=g6g8…70cJ540cKg=335W0g8‡?0e=g6cLg=335E0g:…?0e=g6cMg=33530^2eEg6gAMg:…60`540g=c7g931342g6b4c7g931b4w\\2eNg6gA]e:}31342e=g6cO322eP}31gB6=0e?g6gB32540cGŽ12eQcReSeTg63131eUg631g<33eVg63142;" #(%fl:make-code-emitter
  %fl:cadr %fl:lastcdr %fl:lambda:vars %fl:filter #.pair? lambda #fn(length)
  %fl:keyword-arg? %fl:complex-bindings %fl:lambda:body #fn(map) %fl:extend-env
  %fl:emit optargs %fl:bcode:indexfor %fl:make-perfect-hash-table #.cons #.car
  %fl:iota keyargs %fl:box-vars %fl:emit-optional-arg-inits #{#<unspecified>}#
  %fl:> 255 largc lvargc vargc argc %fl:compile-in ret %fl:lambda:source
  %fl:values #fn(function) %fl:encode-byte-code %fl:bcode:code
  %fl:const-to-idx-vec %fl:bcode:cenv) compile-f-)
	%fl:compile-for #72=#fn("9000r5e0g4316n0e1|}^g2342e2|a322e1|}^g3342e2|a322e1|}^g4342e3|c4322e2|b.42;e5c641;" #(%fl:1arg-lambda?
  %fl:compile-in %fl:bcode:stack %fl:emit for %fl:error "for: third form must be a 1-argument lambda") compile-for)
	%fl:compile-if #73=#fn("@000r4e0|31e0|31e1g331e2g331e3g331F6;0e4g331560e530g:]‚=0e6|}g2g;44;g:^‚=0e6|}g2g<44;e6|}^g:342e7|c8g8332e6|}g2g;342g26<0e7|c9325;0e7|c:g9332e;|g8322e6|}g2g<342e;|g942;" #(%fl:make-label
  %fl:cadr %fl:caddr %fl:cdddr %fl:cadddr %fl:void %fl:compile-in %fl:emit brf
  ret jmp %fl:mark-label) compile-if)
	%fl:compile-in #74=#fn(">000r4g3C6<0e0|}g3]44;g3?6¯0g3`‚:0e1|c242;g3a‚:0e1|c342;g3]‚:0e1|c442;g3^‚:0e1|c542;g3_‚:0e1|c642;e7g3316<0e1|c8g343;c9g3316C0e:|}g2c;c<31L144;e1|c=g343;g3MC@17W02c>g3Me?32@16H02e@g3M31E17;02eAg3M}326=0eB|}g2g344;c>g3Me?32@16>02eCg3eD}3132g86A0e:|}g2eEg83144;g3Mg9cF‚\\0eGeHg331316A0e:|}g2eHg33144;e1|c=eHg33143;g9cI‚=0eJ|}g2g344;g9cK‚>0eL|}g2g3N44;g9cM‚;0eN|}g343;g9cO‚D0ePcQ}g3q2cR|}q242;g9cS‚>0eT|}g2g3N44;g9cU‚>0eV|}g2g3N44;g9cW‚H0eX|}eHg331cKeYg331K44;g9cZ‚K0e[|}eHg331e\\g331e]g33145;g9c^‚H0e:|}]eHg331342e1|c_42;g9c`‚E0ea|}eHg331e\\g33144;g9cb‚A0e:|}g2eEg33144;g9cc‚‚0e:|}^cO_eHg331L3342ede\\g33131660^580eecf312eg|a322e:|}^e\\g331342eg|b/322e1|cc42;eB|}g2g344;" #(%fl:compile-sym
  %fl:emit load0 load1 loadt loadf loadnil %fl:fits-i8 loadi8 #fn(eof-object?)
  %fl:compile-in #fn(top-level-value) eof-object loadv #fn(memq) special-forms
  resolve-global %fl:in-env? %fl:compile-app compile-unknown-call %fl:env-names
  %fl:lower-define quote %fl:self-evaluating? %fl:cadr if %fl:compile-if begin
  %fl:compile-begin prog1 %fl:compile-prog1 lambda %fl:call-with-values
  #fn("7000r0e0~\x7f42;" #(%fl:compile-f-))
  #fn(":000r2e0~c1|332}‡a0e2}c3326;0e4c531540c62c7c8~\x7fq2}322e0~c9c:}3143;c6;" #(%fl:emit
  loadv %fl:length> 255 %fl:error "compile error: a function captures more than 255 variables"
  #{#<unspecified>}# #fn(for-each) #fn("9000r1e0~\x7f|^44;" #(%fl:compile-sym))
  closure #fn(length))) and %fl:compile-and or %fl:compile-or while
  %fl:compile-while %fl:cddr for %fl:compile-for %fl:caddr %fl:cadddr return
  ret set! %fl:compile-set! define trycatch %fl:1arg-lambda? %fl:error "trycatch: second form must be a 1-argument lambda"
  %fl:bcode:stack) compile-in)
	%fl:compile-let #75=#fn("A000r4g3Mg3Ne0|31e1g831e2|}g933e3g;e4e5g831g;32g:33e6|g=`g:g<w342c7g=}M32}NKe8|g>g2e5g831342e9|g<y322e:g<`3216602g2@6<0e;|c<g<43;c=;" #(%fl:bcode:sp
  %fl:cadr %fl:compile-arglist %fl:vars-to-env %fl:complex-bindings %fl:caddr
  %fl:box-vars #fn(append!) %fl:compile-in %fl:bcode:stack %fl:> %fl:emit shift
  #{#<unspecified>}#) compile-let)
	%fl:compile-or #76=#fn(";000r4e0|}g2g3^c146;" #(%fl:compile-short-circuit
							brt) compile-or)
	%fl:compile-prog1 #77=#fn(":000r3e0|}^e1g231342e2g231F6W0e3|a322e4|}^e2g231342e5|c6322e3|b/42;c7;" #(%fl:compile-in
  %fl:cadr %fl:cddr %fl:bcode:stack %fl:compile-begin %fl:emit pop
  #{#<unspecified>}#) compile-prog1)
	%fl:compile-set! #78=#fn("?000r4g2C17902e0c1312e2g2}`33g8c3‚J0e4|}^g3342e5|c6e7g23143;g8M`We8g8N31g96<0e9g8N31590e:|g232g:6o0e5|g9670c;540c<g;332e=|a322e4|}^g3342e=|b/322e5|c>322e5|c?42;e4|}^g3342g97A0e0c@cAg23231540cB2e5|cCg;43;" #(%fl:error
  "set!: second argument must be a symbol" %fl:lookup-sym global %fl:compile-in
  %fl:emit setg resolve-global %fl:vinfo:heap? %fl:vinfo:index %fl:capture-var!
  loada loadc %fl:bcode:stack set-car! car #fn(string)
  "internal error: misallocated var " #{#<unspecified>}# seta) compile-set!)
	%fl:compile-short-circuit #79=#fn("<000r6g3?6=0e0|}g2g444;g3N?6>0e0|}g2g3M44;e1|31e0|}^g3M342e2|a322e3|c4322e3|g5g:332e2|b/322e3|c5322e6|}g2g3Ng4g5362e7|g:42;" #(%fl:compile-in
  %fl:make-label %fl:bcode:stack %fl:emit dup pop %fl:compile-short-circuit
  %fl:mark-label) compile-short-circuit)
	%fl:compile-sym #80=#fn("<000r4e0g2}`33g8c1‚h0e2g231c3g93116=02e4c5g931316@0e6|c7c5g93143;e6|c8g943;g8M`W6]0e6|c9e:g8N31332g316:02e;g8N316:0e6|c<42;c=;e6|c>e?|g232332g316:02e;g8N316:0e6|c<42;c=;" #(%fl:lookup-sym
  global resolve-global #fn(constant?) %fl:printable? #fn(top-level-value)
  %fl:emit loadv loadg loada %fl:vinfo:index %fl:vinfo:heap? car #{#<unspecified>}#
  loadc %fl:capture-var!) compile-sym)
	%fl:compile-thunk #81=#fn(":000r1e0_c1_e2|31L342;" #(%fl:compile-f
							     lambda
							     %fl:lower-define) compile-thunk)
	%fl:compile-while #82=#fn(";000r4e0|31e0|31e1|}^e230342e3|a322e4|g8322e1|}^g2342e5|c6g9332e5|c7322e3|b/322e1|}^g3342e5|c8g8332e4|g942;" #(%fl:make-label
  %fl:compile-in %fl:void %fl:bcode:stack %fl:mark-label %fl:emit brf pop jmp) compile-while)
	%fl:complex-bindings #83=#fn("A000s2c030c030e1|}^^g7g8362c2c3}g7g8q3g2322e4c5g7q1e6g83142;" #(#fn(table)
  %fl:complex-bindings- #fn(for-each) #fn(";000r1e0|~^]\x7fi246;" #(%fl:complex-bindings-))
  %fl:filter #fn("7000r1c0~|42;" #(#fn(has?))) %fl:table.keys) complex-bindings)
	%fl:complex-bindings- #84=#fn("=000r6}…40^;|C6N0g316902c0|}326;0c1g4|]43;c2;|?17802e3|31640^;|Mc4‚z0c0e5|31}326X0c1g5e5|31]332g36A0c1g4e5|31]33540c2540c22e6e7|31}^g3g4g546;|Mc8‚h0c9c:}g4g5q3e;|31322e6e<|31e=}e>|3132^g2@17502g3g4g546;|Mc?<16602|NF6X0e6e5|31}^]g4g5362c9c@}g3g4g5q4eA|3142;e6|M}eB|31g3g4g536cCcD}g3g4g5q4|N32K;" #(#fn(memq)
  #fn(put!) #{#<unspecified>}# %fl:quoted? set! %fl:cadr %fl:complex-bindings-
  %fl:caddr lambda #fn(for-each) #fn(";000r1e0|~^]\x7fi246;" #(%fl:complex-bindings-))
  %fl:lambda:defaults %fl:lambda:body %fl:vars-diff %fl:lambda:vars trycatch #fn(";000r1e0|~^\x7fi2i346;" #(%fl:complex-bindings-))
  %fl:cddr %fl:inlineable? #fn(map) #fn(";000r1e0|~^\x7fi2i346;" #(%fl:complex-bindings-))) complex-bindings-)
	%fl:const-to-idx-vec #85=#fn("9000r1c0e1|3131e2c3g5q1e4|31322g5;" #(#fn(vector.alloc)
  %fl:bcode:nconst %fl:table.foreach #fn("7000r2~}|\\;" #()) %fl:bcode:ctable) const-to-idx-vec)
	%fl:copy-tree #86=#fn("7000r1|?640|;e0|M31e0|N31K;" #(%fl:copy-tree) copy-tree)
	%fl:count #87=#fn("9000r2c0Œ6g6c1g6q1OM2g6M|}`43;" #(#{#<unspecified>}#
							     #fn("9000r3}…50g2;~M|}N|}M31690g2aw540g243;" #() count-)) count)
	%fl:define-head-name #88=#fn("6000r1|F690e0|M41;|;" #(%fl:define-head-name) define-head-name)
	%fl:delete-duplicates #89=#fn(":000r1e0|bD326Q0c130c2Œ6g6c3g5g6q2OM2g6MŽ1|_42;|?640|;|M|Ne4g5g632690e5g641;g5e5g631K;" #(%fl:length>
  #fn(table) #{#<unspecified>}# #fn("8000r2|?680e0}41;c1~|M326:0\x7fM|N}42;c2~|M]332\x7fM|N|M}K42;" #(%fl:reverse!
  #fn(has?) #fn(put!))) %fl:member %fl:delete-duplicates) delete-duplicates)
	%fl:disassemble #90=#fn("P000s1}…C0e0|`322e1302];540c22}Mc3|31c4|31c2c5g6q1m92`g6axc6u2e7c8e9g7`32c:332b4c;g731Œ:c2g:Mg;X6622c<c=g7g:q2^e>33e?g:Mb432690e130540c22`g6axc@u2e7eAg:Mb4x31cBcCg<31cD342g:g:MawOM2g<cEg=cF326P0g9g8e9g7g:M32[312g:g:Mb4wOM5²1cEg=cG326L0g9g8g7g:M[[312g:g:MawOM51cEg=cH326M0e7cIg7g:M[31312g:g:MawOM5g1cEg=cJ326Q0e7cIe9g7g:M3231312g:g:Mb4wOM5=1cEg=cK326¢0e7cIe9g7g:M3231cL322g:g:Mb4wOM2e7cIe9g7g:M3231312g:g:Mb4wOM2g<cM‚Z0e7cL312e7cIe9g7g:M3231cL322g:g:Mb4wOM540c25Â0g=cN=6S0e7cIe9g7g:M3231cL322g:g:Mb4wOM5™0cEg=cO326Z0e7cPeAg:Mb,eQg7g:M32R331322g:g:Mb2wOM5f0cEg=cR326Z0e7cPeAg:Mb,e9g7g:M32R331322g:g:Mb4wOM530^Ž1Ž15%-;" #(%fl:disassemble
  %fl:newline #{#<unspecified>}# #fn(function:code)
  #fn(function:vals) #fn("8000r1|J16602|G@6B0e0c1312e2|~aw42;e3|41;" #(%fl:princ
  "\n" %fl:disassemble %fl:print) print-val)
  #fn("6000r1e0c141;" #(%fl:princ "\t")) %fl:princ "maxstack " %fl:ref-int32-LE
  "\n" #fn(length) #fn(table.foldl) #fn("7000r3g217?02}~\x7fM[<16402|;" #())
  Instructions %fl:> #fn("6000r1e0c141;" #(%fl:princ "\t")) %fl:hex5 ":  " #fn(string)
  "\t" #fn(memq) (loadv.l loadg.l setg.l)
  (loadv loadg setg) (loada seta loadc call tcall list + - * / vector argc
		      vargc loadi8 apply tapply closure box shift)
  #fn(number->string) (loada.l seta.l loadc.l largc lvargc call.l tcall.l box.l)
  (optargs keyargs) " " keyargs brbound (jmp brf brt brne brnn brn) "@"
  %fl:ref-int16-LE (jmp.l brf.l brt.l brne.l brnn.l brn.l)) disassemble)
	%fl:div #91=#fn("7000r2|}V|`X16C02}`X16402a17502b/17402`w;" #() div)
	%fl:double #fn(double) %fl:emit
	#92=#fn("P000s2g2…b0}c0<16C02|`[F16:02|`[Mc1<6;0|`[c2O5:0|`}|`[K\\5Ç1c3}c4326A0e5|g2M32L1m2540c62c7}c832g716<02e9g2Mc:326=0e;g731m1540c6Ž12c7}c<32g716<02e9g2Mc:326=0e;g731m1540c6Ž12}c=‚\\0g2c>>6=0c?m12_m25F0g2c@>6=0cAm12_m2530^540c62}cB‚\\0g2c>>6=0cCm12_m25F0g2c@>6=0cDm12_m2530^540c62|`[F690|`[M530_|`[}cE<16¢02g7cF<16<02e;g831cG<6E0|`g2McHeIg831KK\\5x0g7cF‚B0|`g2McJg8NKK\\5a0g7cK‚B0|`g2McLg8NKK\\5J0g7cG‚B0|`g2McMg8NKK\\530^17a02}cJ<16802g7cG<6B0|`g2McHg8NKK\\5?0|`eN}g2Kg832\\Ž22|;" #(car
  cdr cadr #fn(memq) (loadv loadg setg) %fl:bcode:indexfor #{#<unspecified>}#
  #fn(assq) ((loadv loadv.l) (loadg loadg.l) (setg setg.l) (loada loada.l) (seta
  seta.l)
			     (box box.l)) %fl:> 255 %fl:cadr ((loadc loadc.l))
  loada (0) loada0 (1) loada1 loadc loadc0 loadc1 brf not null? brn %fl:cddr
  brt eq? brne brnn %fl:nreconc) emit)
	%fl:emit-optional-arg-inits #93=#fn("=000r6g2F6³0e0|31e1|c2g4332e1|c3g:332e4|e5}e6g3g432g533^e7g231342e1|c8g4332e1|c9322e:|g:322c;e<g3g432g5326>0e1|c=g433540c>2e?|}g2Ng3g4awg546;c>;" #(%fl:make-label
  %fl:emit brbound brt %fl:compile-in %fl:extend-env %fl:list-head %fl:cadar
  seta pop %fl:mark-label #fn(memq) %fl:list-ref box #{#<unspecified>}#
  %fl:emit-optional-arg-inits) emit-optional-arg-inits)
	%fl:encode-byte-code #94=#fn("S000r1e0|31e1g531e2c3g631b3c3g631b2VT2wc432c3g631`c530c530c630^^c7g<c8322c9g9g8X6Ú12g6g9[m=2g=c:‚P0c;g:g6g9aw[c<g<31332g9b2wm95©1c7g<e=c>e?g76ƒ0g=gDc@‚70cA5p0gDcB‚70cC5d0gDcD‚70cE5X0gDcF‚70cG5L0gDcH‚70cI5@0gDcJ‚70cK540g=Ž1540g=3231322g9awm92g9g8X6:0g6g9[530^m>2cLg=cM326`0c;g;c<g<31g>332c7g<g7670eN540eO`31322g9awm95ì0g=cP‚H0c7g<eNg>31322g9awm95Ï0g>D6È0g=cLg?cQ326H0c7g<eNg>31322g9awm95 0cLg?cR326‚0c7g<eNg>31322g9awm92c7g<eNg6g9[31322g9awm92g=cS‚K0c7g<eNg6g9[31322g9awm9540c95E0c7g<eTg>31322g9awm9Ž1530^5‚.2eUcVg<g7g:q3g;322cWg<41;" #(%fl:reverse!
  %fl:list->vector %fl:>= #fn(length) 65536 #fn(table)
  #fn(buffer) #fn(io.write) #int32(0) #{#<unspecified>}# label #fn(put!)
  #fn(sizeof) %fl:byte #fn(get) Instructions jmp jmp.l brt brt.l brf brf.l brne
  brne.l brnn brnn.l brn brn.l #fn(memq) (jmp brf brt brne brnn brn) %fl:int32
  %fl:int16 brbound (loadv.l loadg.l setg.l loada.l seta.l loadc.l largc lvargc
		     call.l tcall.l box.l) (optargs keyargs) keyargs %fl:uint8
  %fl:table.foreach #fn(";000r2c0~|322c1~\x7f670e2540e3c4i2}32|x3142;" #(#fn(io.seek)
  #fn(io.write) %fl:int32 %fl:int16 #fn(get)))
  #fn(io.tostring!)) encode-byte-code)
	%fl:enum #fn(enum) %fl:env-names
	#95=#fn("7000r1c0c1|42;" #(#fn(map)
				   #fn("7000r1c0e1|42;" #(#fn(map)
							  %fl:vinfo:sym))) env-names)
	%fl:error #96=#fn("9000s0c0c1|K41;" #(#fn(raise) error) error) %fl:eval
	#97=#fn("7000r1e0e1|313140;" #(%fl:compile-thunk %fl:expand) eval)
	%fl:even? #98=#fn("7000r1c0|a32`W;" #(#fn(logand)) even?) %fl:every
	#99=#fn("7000r2}?17D02|}M3116:02e0|}N42;" #(%fl:every) every)
	%fl:expand #100=#fn("F000r1c0c0c0c0c0c0c0c0c0c0c0Œ5Œ6Œ7Œ8Œ9Œ:Œ;Œ<Œ=Œ>Œ?g5c1OM2g6c2g6q1OM2g7c3L1OM2g8c4g5g6g?g7q4OM2g9c5g?g9q2OM2g:c6g:q1OM2g;c7g:g9g8q3OM2g<c8g?g:g9g8q4OM2g=c9g8g?q2OM2g>c:OM2g?c;g?g;g<g>g=q5OM2g?M|_42;" #(#{#<unspecified>}#
  #fn("7000r2e0|31E17902c1|}32@;" #(resolve-global #fn(assq)) top?)
  #fn("8000r1|?640|;|c0>640|;|MF16;02e1|31c2<6F0c3~Me4|3131~M|N3142;|M~M|N31K;" #(((begin))
  %fl:caar begin #fn(append) %fl:cdar) splice-begin) *expanded* #fn("A000r2|?640|;~Mc0}326:0\x7fM|31530|~Mc1}32g76;0e2g631530_c3c4c5g832}32Œ9g77A0c4c6i2g9q2g642;c7Œ:g:c8g:i2g9i3q4OM2g:MŽ1g631g:c7g;F6c02i3Me9g;31<7C0g;i2Mg;Mg9M32O5;0g;e:g;31O2g;Nm;5û/2g:;" #(begin
  define %fl:get-defined-vars #fn(nconc) #fn(map) #.list #fn("7000r1~M|\x7fM42;" #())
  #{#<unspecified>}# #fn(";000r1|?640|;|MF16;02c0e1|31<6<0|M~M|N31K;\x7fM|Mi2M32i2c2c3c4e5g53132i2M32OM2i3Mg5K~M|N31K;" #(define
  %fl:caar #fn(nconc) #fn(map) #.list %fl:get-defined-vars)) %fl:caar %fl:cdar) expand-body)
  #fn("9000r2|?640|;|MF16702|MNF6F0e0|31~Me1|31}32L2540|M\x7fM|N}32K;" #(%fl:caar
  %fl:cadar) expand-lambda-list) #fn("7000r1|?660|L1;|MF6?0e0|31~M|N31K;|M~M|N31K;" #(%fl:caar) l-vars)
  #fn("?000r2e0|31e1|31e2|31~Me0|3131c3c4c5g932}32c3c6L1\x7fMg6g:32L1i2Mg8g:32g744;" #(%fl:cadr
  %fl:lastcdr %fl:cddr #fn(nconc) #fn(map) #.list lambda) expand-lambda)
  #fn("?000r2e0|31m02|NA17902e1|31?6P0e2|31…40|;c3e1|31~Me4|31}32L3;e5|31e6|31e2|31\x7fMe5|3131c7c8c9g932}32c7c3L1g7i2Mg6g:32KL1i3Mg8g:3243;" #(%fl:uncurry-define
  %fl:cadr %fl:cddr define %fl:caddr %fl:cdadr %fl:caadr #fn(nconc)
  #fn(map) #.list) expand-define) #fn("=000r2e0|31c1~Me2|31c3c4c5\x7f}q2g632}3232K;" #(%fl:cadr
  begin %fl:cddr #fn(nconc) #fn(map) #fn("9000r1|Me0~Me1|31\x7f323130\x7fL3;" #(%fl:compile-thunk
  %fl:cadr))) expand-let-syntax) #fn("5000r2|;" #() local-expansion-env)
  #fn("=000r2|?640|;|Mc0g6}32c1~}|q3c2g6c3326U0g6g9c4=640|;g9c5=690\x7fM|}42;i2M|}42;g716702g7NF6M0~Me6g731|NQ2i3Me7g731}3242;g717C02g6C@17:02e8g631E670g840;e9|31g96?0~Me:g9|32}42;g6c4‚40|;g6c5‚90\x7fM|}42;g6c;‚:0i2M|}42;g6c<‚:0i4M|}42;g840;" #(#fn(assq)
  #fn(":000r0c0Œ4g4c1~\x7fg4q3OM2g4MŽ1i241;" #(#{#<unspecified>}# #fn("8000r1|?640|;|M?670|M590~M|M\x7f32i2M|N31K;" #())))
  #fn(memq) (quote lambda define) quote lambda %fl:cadr %fl:caddr
  resolve-global %fl:macrocall? %fl:expand-macro-call define let-syntax) expand-in)) expand)
	%fl:expand-define #101=#fn("?000r1e0|31e1|31F6:0e1|315O0e0|31C6;0e230L15=0e3c4e5|3132g5C6<0c6g5g6ML3;c6g5Mc7c8L1g5NL1c9g631g5M34L3;" #(%fl:cadr
  %fl:cddr %fl:void %fl:error "compile error: invalid syntax "
  %fl:print-to-string set! #fn(nconc) lambda #fn(copy-list)) expand-define)
	%fl:expand-macro-call #102=#fn("7000r2e06<0c1|}q2c2t;|}Nv2;" #(*defer-macro-errors*
  #fn("6000r0~\x7fNv2;" #()) #fn("7000r1c0c1|L2L2;" #(raise quote))) expand-macro-call)
	%fl:extend-env #103=#fn("8000r3e0}g2`33|K;" #(%fl:vars-to-env) extend-env)
	%fl:filter #104=#fn("9000r2c0c1|q1m62g6|}_L143;" #(#{#<unspecified>}#
							   #fn("8000r3g2c0}F6R02~}M316?0g2}M_KPNm2540c02}Nm15\x0d/2N;" #(#{#<unspecified>}#) filter-)) filter)
	%fl:fits-i8 #105=#fn("7000r1|I16F02e0|b°3216:02e1|b¯42;" #(%fl:>=
								   %fl:<=) fits-i8)
	%fl:float #fn(float) %fl:foldl
	#106=#fn("9000r3g2…40};e0||g2M}32g2N43;" #(%fl:foldl) foldl) %fl:foldr
	#107=#fn(":000r3g2…40};|g2Me0|}g2N3342;" #(%fl:foldr) foldr)
	%fl:function-source #108=#fn("8000r1|J16q02|G@16i02c0|31e1c2g531`3216T02g5c2g531ax[g6F16A02g6Mc3<16602g6N;" #(#fn(function:vals)
  %fl:> #fn(length) %source) function-source)
	%fl:get-defined-vars #109=#fn("7000r1e0~M|3141;" #(%fl:delete-duplicates) #(#0=(#fn("8000r1|?640_;|Mc0<16602|NF6u0e1|31C16:02e1|31L117^02e1|31F16M02e2e1|3131C16>02e2e1|3131L117402_;|Mc3‚?0c4c5~M|N32v2;_;" #(define
  %fl:cadr %fl:define-head-name begin #fn(nconc)
  #fn(map)) #(#0#)))))
	%fl:hex5 #110=#fn("8000r1e0c1|b@32b5c243;" #(%fl:string.lpad #fn(number->string)
						     #\0) hex5)
	%fl:identity #111=#fn("5000r1|;" #() identity) %fl:in-env?
	#112=#fn("7000r2}F16F02c0|}M3217:02e1|}N42;" #(#fn(assq) %fl:in-env?) in-env?)
	%fl:index-of #113=#fn("9000r3}…40^;|}M‚50g2;e0|}Ng2aw43;" #(%fl:index-of) index-of)
	%fl:inlineable? #114=#fn("9000r1|Mg5F16€02g5Mc0<16u02e1e2g5313116f02e3c4e2g5313216U02e5e2g531c632@16C02e7e2g531c8|N3142;" #(lambda
  %fl:list? %fl:cadr %fl:every #.symbol? %fl:length> 255 %fl:length= #fn(length)) inlineable?)
	%fl:int16 #fn(int16) %fl:int32
	#fn(int32) %fl:int64 #fn(int64) %fl:int8
	#fn(int8) %fl:io.readall #115=#fn("8000r1c030c1g5|322c2g531g6c3>16802c4|31670c540;g6;" #(#fn(buffer)
  #fn(io.copy) #fn(io.tostring!) "" #fn(io.eof?)
  #fn(eof-object)) io.readall)
	%fl:io.readline #116=#fn("7000r1c0|c142;" #(#fn(io.readuntil) #\newline) io.readline)
	%fl:io.readlines #117=#fn("7000r1e0e1|42;" #(%fl:read-all-of
						     %fl:io.readline) io.readlines)
	%fl:iota #118=#fn("7000r1e0e1|42;" #(%fl:map-int %fl:identity) iota)
	%fl:keyword->symbol #119=#fn("=000r1c0|316Q0c1c2|31c3g6`c4g6c5g6313233Ž141;|;" #(#fn(keyword?)
  #fn(symbol) #fn(string) #fn(string.sub)
  #fn(string.dec) #fn(length)) keyword->symbol)
	%fl:keyword-arg? #120=#fn("6000r1|F16902c0|M41;" #(#fn(keyword?)) keyword-arg?)
	%fl:lambda-arg-names #121=#fn("8000r1e0c1e2|3142;" #(%fl:map! #fn("6000r1|F690e0|M41;|;" #(%fl:keyword->symbol))
							     %fl:to-proper) lambda-arg-names)
	%fl:lambda-vars #122=#fn(":000r1c0Œ5g5c1g5q1OM2g5M||^^342e2|41;" #(#{#<unspecified>}#
  #fn(":000r4|A17502|C640];|F16602|MC6T0g217502g36<0e0c1}c243;~M|N}g2g344;|F16602|MF6‰0e3|Mb23216902e4|31C660^5=0e0c5|Mc6}342c7e4|31316=0~M|N}g2]44;g36<0e0c1}c843;~M|N}]g344;|F6>0e0c9|Mc6}44;|}‚:0e0c1}42;e0c9|c6}44;" #(%fl:error
  "compile error: invalid argument list "
  ". optional arguments must come after required." %fl:length= %fl:caar "compile error: invalid optional argument "
  " in list " #fn(keyword?) ". keyword arguments must come last."
  "compile error: invalid formal argument ") check-formals)
  %fl:lambda-arg-names) lambda-vars)
	%fl:lambda:body #123=#fn("6000r1e0|41;" #(%fl:caddr) lambda:body)
	%fl:lambda:defaults #124=#fn("8000r1c0Œ5g5c1g5q1OM2g5MŽ1e2|3141;" #(#{#<unspecified>}#
  #fn("7000r1|?640_;|MF6?0e0|31~M|N31K;~M|N41;" #(%fl:cadar)) %fl:cadr) lambda:defaults)
	%fl:lambda:source #125=#fn("7000r1e0|31g5F16M02g5MF16D02e1g531c2<16602g5M;" #(%fl:cdddr
  %fl:caar %source) lambda:source)
	%fl:lambda:vars #126=#fn("7000r1e0e1|3141;" #(%fl:lambda-vars %fl:cadr) lambda:vars)
	%fl:last-pair #127=#fn("6000r1|N?640|;e0|N41;" #(%fl:last-pair) last-pair)
	%fl:lastcdr #128=#fn("6000r1|?640|;e0|31N;" #(%fl:last-pair) lastcdr)
	%fl:length= #129=#fn("8000r2}`X640^;}`W650|?;|?660}`W;e0|N}ax42;" #(%fl:length=) length=)
	%fl:length> #130=#fn("8000r2}`X640|;}`W6;0|F16402|;|?660}`X;e0|N}ax42;" #(%fl:length>) length>)
	%fl:list->vector #131=#fn("6000r1c0|v2;" #(#.vector) list->vector)
	%fl:list-head #132=#fn("9000r2e0}`32640_;|Me1|N}ax32K;" #(%fl:<=
								  %fl:list-head) list-head)
	%fl:list-ref #133=#fn("7000r2e0|}32M;" #(%fl:list-tail) list-ref)
	%fl:list-tail #134=#fn("8000r2e0}`32640|;e1|N}ax42;" #(%fl:<=
							       %fl:list-tail) list-tail)
	%fl:list? #135=#fn("6000r1|A17@02|F16902e0|N41;" #(%fl:list?) list?)
	%fl:load #136=#fn("9000r1c0|c132c2g5q1c3g5|q2t;" #(#fn(file) :read #fn("9000r0c0Œ4g4c1~g4q2OM2g4MŽ1c0c0c043;" #(#{#<unspecified>}#
  #fn("9000r3c0~317B0\x7fMc1~31|e2}3143;c3~312e2}41;" #(#fn(io.eof?)
							#fn(read)
							%fl:load-process #fn(io.close)))))
							   #fn("8000r1c0~312c1c2\x7f|L341;" #(#fn(io.close)
  #fn(raise) load-error))) load)
	%fl:load-process #137=#fn("6000r1e0|41;" #(%fl:eval) load-process)
	%fl:long #fn(long) %fl:lookup-sym
	#138=#fn(";000r3}…50c0;}Mc1|g732g8680g2g8K;e2|}Ng2aw43;" #(global #fn(assq)
								   %fl:lookup-sym) lookup-sym)
	%fl:lower-arglist #139=#fn("8000r1|?640|;|MF16902e0|31F6J0e1|31e2e3|3131L2e4|N31K;|Me4|N31K;" #(%fl:cdar
  %fl:caar %fl:lower-define %fl:cadar %fl:lower-arglist) lower-arglist)
	%fl:lower-define #140=#fn("@000r1c0c1m52|?17802e2|31640|;|Mc3‚<0e4e5|3141;|Mc6‚•0e76p0c8c6L1e9e:|3131L1g5|31L1c8c;L1c6L1e:|31L1e<e=|313134L1e>|3145;c8c6L1e9e:|3131L1g5|31L1e>|3144;c?e4|42;" #(#{#<unspecified>}#
  #fn("=000r1e0|31F6N0e1|31F6=0c2e0|31K570e3|31560e430e5g531e6g531g6…50g7;c7g6g7L3c8c9g632K;" #(%fl:cddr
  %fl:cdddr begin %fl:caddr %fl:void %fl:get-defined-vars %fl:lower-define
  lambda #fn(map) #fn("5000r1e040;" #(%fl:void))) lambda-body) %fl:quoted?
  define %fl:lower-define %fl:expand-define lambda *keep-source* #fn(nconc)
  %fl:lower-arglist %fl:cadr %source %fl:proper-part %fl:cddr %fl:lastcdr #fn(map)) lower-define)
	%fl:macrocall? #141=#fn("7000r1|MC16=02e0e1|M3141;" #(%fl:symbol-syntax
							      resolve-global) macrocall?)
	%fl:macroexpand-1 #142=#fn("7000r1|?640|;e0|31g5690g5|Nv2;|;" #(%fl:macrocall?) macroexpand-1)
	%fl:make-code-emitter #143=#fn("9000r0_c030`_^Z5;" #(#fn(table)) make-code-emitter)
	%fl:make-label #144=#fn("5000r1c040;" #(#fn(gensym)) make-label)
	%fl:make-perfect-hash-table #145=#fn(";000r1c0Œ5g5c1OM2c0Œ6g6c2g5g6|q3OM2g6MŽ1c3|3141;" #(#{#<unspecified>}#
  #fn("8000r2e0e1c2|3131}42;" #(%fl:mod0 %fl:abs #fn(hash)) $hash-keyword)
  #fn("=000r1c0b2|T2^32c1Œ6g6c2~|g5\x7fg6q5OM2g6MŽ1i241;" #(#fn(vector.alloc)
							    #{#<unspecified>}#
							    #fn(":000r1|F6p0e0|31b2~Mg5\x7f32T2i2g6[6;0i3M\x7faw41;i2g6g5\\2i2g6awe1|31\\2i4M|N41;i2;" #(%fl:caar
  %fl:cdar)))) #fn(length)) make-perfect-hash-table)
	%fl:make-system-image #146=#fn("<000r1c0|c1c2c334c4e5e6]k52]k62c7g7g8q2c8g6g5q2c9g9q1tg9302;" #(#fn(file)
  :write :create :truncate (*linefeed* *directory-separator* *argv* that
				       *print-pretty* *print-width*
				       *print-readably* *print-level*
				       *print-length* *os-name*) *print-pretty*
  *print-readably* #fn("5000r0~k02\x7fk1;" #(*print-pretty* *print-readably*))
  #fn("=000r0e0c1~q1e2c3303132c4c5c6c7g4c6c8g43233Q2\x7f322c9\x7fe:32Ž12c;\x7f41;" #(%fl:filter
  #fn("8000r1|E16u02c0|31@16j02c1|31G@17C02c2|31c2c1|3131>@16I02c3|~32@16=02c4c1|3131@;" #(#fn(constant?)
  #fn(top-level-value) #fn(string) #fn(memq)
  #fn(iostream?))) %fl:simple-sort #fn(environment)
  #fn(write) #fn(nconc) #fn(map) #.list #fn(top-level-value)
  #fn(io.write) *linefeed* #fn(io.close)))
  #fn("6000r1~302c0|41;" #(#fn(raise)))) make-system-image)
	%fl:map! #147=#fn("8000r2}c0}F6B02}|}M31O2}Nm15\x1d/2;" #(#{#<unspecified>}#) map!)
	%fl:map-int #148=#fn(";000r2e0}`32640_;|`31_K_Œ7g7g6OM2a}axc1g7|q2u2g6;" #(%fl:<=
  #fn("7000r1~M\x7f|31_KP2~~MNOM;" #())) map-int)
	%fl:mark-label #149=#fn("8000r2e0|c1}43;" #(%fl:emit label) mark-label)
	%fl:max #150=#fn(";000s1}…40|;e0c1|}43;" #(%fl:foldl #fn("6000r2|}X640};|;" #())) max)
	%fl:member #151=#fn("7000r2}?640^;}M|>640};e0|}N42;" #(%fl:member) member)
	%fl:memv #152=#fn("7000r2}?640^;}M|=640};e0|}N42;" #(%fl:memv) memv)
	%fl:min #153=#fn(";000s1}…40|;e0c1|}43;" #(%fl:foldl #fn("6000r2|}X640|;};" #())) min)
	%fl:mod #154=#fn("8000r2|e0|}32}T2x;" #(%fl:div) mod) %fl:mod0
	#155=#fn("7000r2||}V}T2x;" #() mod0) %fl:nan? #156=#fn("6000r1|c0>17702|c1>;" #(+nan.0
  -nan.0) nan?)
	%fl:nary-compare #157=#fn("8000r2}A17Q02}NA17I02|}Me0}313216:02e1|}N42;" #(%fl:cadr
  %fl:nary-compare) nary-compare)
	%fl:nary< #158=#fn("9000s0e0c1|42;" #(%fl:nary-compare #fn("6000r2|}X;" #())) nary<)
	%fl:nary= #159=#fn("9000s0e0c1|42;" #(%fl:nary-compare #fn("6000r2|}W;" #())) nary=)
	%fl:negative? #160=#fn("6000r1|`X;" #() negative?) %fl:nestlist
	#161=#fn(":000r3e0g2`32640_;}e1||}31g2ax33K;" #(%fl:<= %fl:nestlist) nestlist)
	%fl:newline #162=#fn("8000‰00001000Š0000770e0m02c1|e2322];" #(*output-stream*
  #fn(io.write) *linefeed*) newline)
	%fl:nreconc #163=#fn("7000r2e0}|42;" #(%fl:reverse!-) nreconc) %fl:odd?
	#164=#fn("6000r1e0|31@;" #(%fl:even?) odd?) %fl:positive? #165=#fn("7000r1e0|`42;" #(%fl:>) positive?)
	%fl:princ #166=#fn(";000s0e0^k02c1g5q1c2|q1c3g6q1tg6302;" #(*print-readably*
								    #fn("5000r0~k0;" #(*print-readably*))
								    #fn("7000r0c0c1~42;" #(#fn(for-each)
  #fn(write))) #fn("6000r1~302c0|41;" #(#fn(raise)))) princ)
	%fl:print #167=#fn("9000s0c0c1|42;" #(#fn(for-each)
					      #fn(write)) print)
	%fl:print-exception #168=#fn("=000r1|F16D02|Mc0<16:02e1|b4326S0e2c3e4|31c5e6|31c7352e8e9|31315-0|F16D02|Mc:<16:02e1|b4326Q0e2e4|31c;e9|31c<342e8e6|31315ó0|F16@02|Mc=<16602|NF6B0e2c>e4|31c?335Ì0|F16802|Mc@<6B0e2cA312e2|NQ25­0|F16802|McB<6J0eCe6|31312e2cDe4|31325†0eE|3116:02e1|b2326f0e8|M312e2cF312e4|31cGg53117602g5C670e2540e8g531Ž15>0e2cH312e8|312e2eI41;" #(type-error
  %fl:length= %fl:princ "type error: " %fl:cadr ": expected " %fl:caddr ", got "
  %fl:print %fl:cadddr bounds-error ": index " " out of bounds for "
  unbound-error "eval: variable " " has no value" error "error: " load-error
  %fl:print-exception "in file " %fl:list? ": " #fn(string?)
  "*** Unhandled exception: " *linefeed*) print-exception)
	%fl:print-stack-trace #169=#fn("?000r1c0c0Œ5Œ6g5c1g5q1OM2g6c2g5q1OM2e3e4|e5670b5540b43231e6e7c8c9c:303232`Œ9c;c<g9g6g8q3g742;" #(#{#<unspecified>}#
  #fn("=000r3c0|31g2Kc1|31c1}31‚?0c2c3c4g7L341;c5|31`e6c7g83131c8g8~}g7q4u;" #(#fn(function:name)
  #fn(function:code) #fn(raise) thrown-value ffound #fn(function:vals) %fl:1-
  #fn(length) #fn("8000r1e0~|[316>0\x7fM~|[i2i343;c1;" #(%fl:closure?
							 #{#<unspecified>}#))) find-in-f)
  #fn(";000r2c0~|}q3c1tg66I0c2e3c4c5e6g63132c73241;c8;" #(#fn("8000r0c0c1~\x7fq2i2322^;" #(#fn(for-each)
  #fn("8000r1~M|\x7f_43;" #()))) #fn("6000r1|F16E02|Mc0<16;02e1|31c2<680e3|41;c4|41;" #(thrown-value
  %fl:cadr ffound %fl:caddr #fn(raise))) #fn(symbol) %fl:string.join #fn(map)
							  #fn(string)
							  %fl:reverse! "/"
							  lambda) fn-name)
  %fl:reverse! %fl:list-tail *interactive* %fl:filter %fl:closure? #fn(map)
  #fn("6000r1|E16802c0|41;" #(#fn(top-level-value)))
  #fn(environment) #fn(for-each) #fn("8000r1e0c1~Mc2332e3\x7fM|`[i232e4|31NK312e5302~~MawOM;" #(%fl:princ
  "#" " " %fl:print %fl:vector->list %fl:newline))) print-stack-trace)
	%fl:print-to-string #170=#fn("8000r1c030c1|g5322c2g541;" #(#fn(buffer)
								   #fn(write)
								   #fn(io.tostring!)) print-to-string)
	%fl:printable? #171=#fn("6000r1c0|3117802c1|31@;" #(#fn(iostream?)
							    #fn(eof-object?)) printable?)
	%fl:proper-part #172=#fn("7000r1|F6<0|Me0|N31K;_;" #(%fl:proper-part) proper-part)
	%fl:quote-value #173=#fn("6000r1e0|31640|;c1|L2;" #(%fl:self-evaluating?
							    quote) quote-value)
	%fl:quoted? #174=#fn("6000r1|Mc0<;" #(quote) quoted?) %fl:random
	#175=#fn("7000r1c0|316<0e1c230|42;c330|T2;" #(#fn(integer?) %fl:mod #fn(rand)
						      #fn(rand.double)) random)
	%fl:read-all #176=#fn("7000r1e0c1|42;" #(%fl:read-all-of #fn(read)) read-all)
	%fl:read-all-of #177=#fn(":000r2c0Œ6g6c1}g6|q3OM2g6MŽ1_|}3142;" #(#{#<unspecified>}#
  #fn("8000r2c0~31680e1|41;\x7fM}|Ki2~3142;" #(#fn(io.eof?) %fl:reverse!))) read-all-of)
	%fl:ref-int16-LE #178=#fn(":000r2e0c1|}`w[`32c1|}aw[b832w41;" #(%fl:int16
  #fn(ash)) ref-int16-LE)
	%fl:ref-int32-LE #179=#fn("<000r2e0c1|}`w[`32c1|}aw[b832c1|}b2w[b@32c1|}b3w[bH32R441;" #(%fl:int32
  #fn(ash)) ref-int32-LE)
	%fl:repl #180=#fn(":000r0c0c0Œ4Œ5g4c1OM2g5c2g4g5q2OM2g5M302e340;" #(#{#<unspecified>}#
  #fn("8000r0e0c1312c2e3312c4c5tc6e731@16F02e8g431e9g5312g5k:2];" #(%fl:princ
								    "> " #fn(io.flush)
								    *output-stream*
								    #fn("5000r0c040;" #(#fn(read)))
								    #fn("6000r1c0e1312c2|41;" #(#fn(io.discardbuffer)
  *input-stream* #fn(raise))) #fn(io.eof?) *input-stream* %fl:load-process
								    %fl:print
								    that) prompt)
  #fn("6000r0c0~q1c1t6<0e2302\x7fM40;^;" #(#fn("6000r0~M3016702e040;" #(%fl:newline))
					   #fn("6000r1e0|312];" #(%fl:top-level-exception-handler))
					   %fl:newline) reploop) %fl:newline) repl)
	%fl:revappend #181=#fn("7000r2e0}|42;" #(%fl:reverse-) revappend)
	%fl:reverse #182=#fn("7000r1e0_|42;" #(%fl:reverse-) reverse)
	%fl:reverse! #183=#fn("7000r1e0_|42;" #(%fl:reverse!-) reverse!)
	%fl:reverse!- #184=#fn("8000r2c0}F6B02}N}|}m02P2m15\x1d/2|;" #(#{#<unspecified>}#) reverse!-)
	%fl:reverse- #185=#fn("7000r2}…40|;e0}M|K}N42;" #(%fl:reverse-) reverse-)
	%fl:self-evaluating? #186=#fn("7000r1|?16602|C@17K02c0|3116A02|C16:02|c1|31<;" #(#fn(constant?)
  #fn(top-level-value)) self-evaluating?)
	%fl:separate #187=#fn(":000r2c0c1m62g6|}_L1_L144;" #(#{#<unspecified>}#
							     #fn("9000r4g2g3Kc0}F6Z02|}M316?0g2}M_KPNm25<0g3}M_KPNm32}Nm15\x05/2e1g8MNg8NN42;" #(#{#<unspecified>}#
  %fl:values) separate-)) separate)
	%fl:set-syntax! #188=#fn("8000r2c0e1e2|31}43;" #(#fn(put!)
							 *syntax-environment*
							 resolve-global) set-syntax!)
	%fl:simple-compare #189=#fn("<000r2|Mc0g6c13216Æ02e2|b33216º02e3g6}32@16­02e4g631g6<16Ÿ02c5Œ7g7c6}g7q2OM2g7MŽ1c7311602c8e9|31e:|31g7g83116j02g7g93116_02g6g:c;=6;0c<g9g8L3;g:c==6;0e>g8g942;e>g9g842;" #(#fn(memq)
  (> <= >=) %fl:length= %fl:in-env? resolve-global #{#<unspecified>}# #fn("7000r1|A17T02e0|M~32@16G02e1|M31|M<16902\x7fM|N41;" #(%fl:in-env?
  resolve-global)) (< = number?) #fn("6000r1|?17802|Mc0<;" #(quote)) %fl:cadr
  %fl:caddr > < <= %fl:compare<=) simple-compare)
	%fl:simple-sort #190=#fn("9000r1|A17602|NA640|;|Me0c1g5|q2c2g5q142;" #(%fl:call-with-values
  #fn("7000r0e0c1~q1\x7fN42;" #(%fl:separate #fn("6000r1|~X;" #())))
  #fn("9000r2c0e1|31~L1e1}3143;" #(#fn(nconc) %fl:simple-sort))) simple-sort)
	%fl:splice-form? #191=#fn("7000r1|F16X02|Mc0<17N02|Mc1<17D02|Mc2<16:02e3|b23217702|c2<;" #(unquote-splicing
  unquote-nsplicing unquote %fl:length>) splice-form?)
	%fl:string.join #192=#fn("9000r2|…50c0;c130c2g6|M322c3c4g6}q2|N322c5g641;" #(""
  #fn(buffer) #fn(io.write) #fn(for-each)
  #fn("7000r1c0~\x7f322c0~|42;" #(#fn(io.write)))
  #fn(io.tostring!)) string.join)
	%fl:string.lpad #193=#fn(":000r3c0e1g2}c2|31x32|42;" #(#fn(string)
							       %fl:string.rep
							       #fn(string.count)) string.lpad)
	%fl:string.map #194=#fn("=000r2c030c1}31`c2g8g7X6P02c3g6|c4}g83231322c5}g832m85\x0c/Ž12c6g641;" #(#fn(buffer)
  #fn(length) #{#<unspecified>}# #fn(io.putc)
  #fn(string.char) #fn(string.inc) #fn(io.tostring!)) string.map)
	%fl:string.rep #195=#fn(":000r2}b4X6`0e0}`32650c1;}aW680c2|41;}b2W690c2||42;c2|||43;e3}316@0c2|e4|}ax3242;e4c2||32}b2U242;" #(%fl:<=
  "" #fn(string) %fl:odd? %fl:string.rep) string.rep)
	%fl:string.rpad #196=#fn(";000r3c0|e1g2}c2|31x3242;" #(#fn(string)
							       %fl:string.rep
							       #fn(string.count)) string.rpad)
	%fl:string.tail #197=#fn(":000r2c0|c1|`}3342;" #(#fn(string.sub)
							 #fn(string.inc)) string.tail)
	%fl:string.trim #198=#fn(">000r3c0c0Œ7Œ8g7c1g7q1OM2g8c2g8q1OM2c3|31c4|g7M|}`g934g8M|g2g93343;" #(#{#<unspecified>}#
  #fn(":000r4g2g3X16?02c0}c1|g232326B0~M|}c2|g232g344;g2;" #(#fn(string.find)
							     #fn(string.char)
							     #fn(string.inc)) trim-start)
  #fn(";000r3e0g2`3216D02c1}c2|c3|g23232326@0~M|}c3|g23243;g2;" #(%fl:> #fn(string.find)
								  #fn(string.char)
								  #fn(string.dec)) trim-end)
  #fn(length) #fn(string.sub)) string.trim)
	%fl:symbol-syntax #199=#fn("8000r1c0e1|^43;" #(#fn(get)
						       *syntax-environment*) symbol-syntax)
	%fl:table.clone #200=#fn("9000r1c030c1c2g5q1_|332g5;" #(#fn(table)
								#fn(table.foldl)
								#fn("8000r3c0~|}43;" #(#fn(put!)))) table.clone)
	%fl:table.foreach #201=#fn("8000r2c0c1|q1_}43;" #(#fn(table.foldl)
							  #fn("7000r3~|}322];" #())) table.foreach)
	%fl:table.invert #202=#fn("9000r1c030c1c2g5q1_|332g5;" #(#fn(table)
								 #fn(table.foldl)
								 #fn("8000r3c0~}|43;" #(#fn(put!)))) table.invert)
	%fl:table.keys #203=#fn("8000r1c0c1_|43;" #(#fn(table.foldl)
						    #fn("6000r3|g2K;" #())) table.keys)
	%fl:table.pairs #204=#fn("8000r1c0c1_|43;" #(#fn(table.foldl)
						     #fn("6000r3|}Kg2K;" #())) table.pairs)
	%fl:table.values #205=#fn("8000r1c0c1_|43;" #(#fn(table.foldl)
						      #fn("6000r3}g2K;" #())) table.values)
	%fl:to-proper #206=#fn("7000r1|…40|;|?660|L1;|Me0|N31K;" #(%fl:to-proper) to-proper)
	%fl:top-level-exception-handler #207=#fn("9000r1e0e1k02c2g5q1c3|q1c4g6q1tg6302;" #(*output-stream*
  *stderr* #fn("5000r0~k0;" #(*output-stream*))
  #fn("6000r0e0~312e1c23041;" #(%fl:print-exception %fl:print-stack-trace
				#fn(stacktrace)))
  #fn("6000r1~302c0|41;" #(#fn(raise)))) top-level-exception-handler)
	%fl:trace #208=#fn("A000r1c0|31c130e2g5317g0c3|e4c5g6c6c7c8c9|L2g6L3L2c:L1c;c9g5L2g6L3L4L33132540c<Ž1Ž12c=;" #(#fn(top-level-value)
  #fn(gensym) %fl:traced? #fn(set-top-level-value!) %fl:eval lambda begin write
  cons quote newline apply #{#<unspecified>}# ok) trace)
	%fl:traced? #209=#fn("7000r1e0|3116?02c1|31c1~M31>;" #(%fl:closure? #fn(function:code)) #((#fn("9000s0c0c1|K312e2302c3|v2;" #(#fn(write)
  x %fl:newline #.apply)))))
	%fl:uint16 #fn(uint16) %fl:uint32
	#fn(uint32) %fl:uint64 #fn(uint64) %fl:uint8
	#fn(uint8) %fl:ulong #fn(ulong) %fl:uncurry-define
	#210=#fn("<000r1|NF16D02e0|31F16902e1|31F6P0e2|Me1|31c3c4e5|31e6|3133L341;|;" #(%fl:cadr
  %fl:caadr %fl:uncurry-define #fn(list*) lambda %fl:cdadr %fl:cddr) uncurry-define)
	%fl:untrace #211=#fn("9000r1c0|31e1g5316A0c2|c3g531b2[42;c4;" #(#fn(top-level-value)
  %fl:traced? #fn(set-top-level-value!) #fn(function:vals) #{#<unspecified>}#) untrace)
	%fl:values #212=#fn("8000s0|F16602|NA650|M;~|K;" #() #(#1#))
	%fl:vars-diff #213=#fn("8000r2|…40_;c0|M}326:0e1|N}42;|Me1|N}32K;" #(#fn(memq)
  %fl:vars-diff) vars-diff)
	%fl:vars-to-env #214=#fn(":000r3c0c1}g2q2|e2c3|313143;" #(#fn(map)
								  #fn("9000r2e0|c1|~32@@}\x7fw43;" #(%fl:vinfo
  #fn(memq))) %fl:iota #fn(length)) vars-to-env)
	%fl:vector->list #215=#fn("<000r1c0|31_Œ6ag5c1g6|g5q3u2g6M;" #(#fn(length)
  #fn("8000r1~\x7fi2|x[~MKOM;" #())) vector->list)
	%fl:vector.map #216=#fn("<000r2c0}31c1g631`g6axc2g7|}q3u2g7;" #(#fn(length)
  #fn(vector.alloc) #fn("9000r1~|\x7fi2|[31\\;" #())) vector.map)
	%fl:vinfo #217=#fn("7000r3|}g2L3;" #() vinfo) %fl:vinfo:heap?
	#2# %fl:vinfo:index #3# %fl:vinfo:sym #.car %fl:void
	#218=#fn("5000r0c0;" #(#{#<unspecified>}#) void) %fl:wchar #fn(wchar)
	%fl:zero? #219=#fn("6000r1|`W;" #() zero?) *banner*
	";  _\n; |_ _ _ |_ _ |  . _ _\n; | (-||||_(_)|__|_)|_)\n;-------------------|----------------------------------------------------------\n\n"
	*builtins* #(0 0 0 0 0 0 0 0 0 0 0 0 #fn("6000r2|}<;" #())
		     #fn("6000r2|}=;" #())
		     #fn("6000r2|}>;" #())
		     #fn("5000r1|?;" #())
		     #fn("5000r1|@;" #())
		     #fn("5000r1|A;" #())
		     #fn("5000r1|B;" #())
		     #fn("5000r1|C;" #())
		     #fn("5000r1|D;" #())
		     #fn("5000r1|E;" #())
		     #fn("5000r1|F;" #())
		     #fn("5000r1|G;" #())
		     #fn("5000r1|H;" #())
		     #fn("5000r1|I;" #())
		     #fn("5000r1|J;" #())
		     #fn("6000r2|}K;" #())
		     #fn("7000s0|;" #()) #fn("5000r1|M;" #())
		     #fn("5000r1|N;" #())
		     #fn("6000r2|}O;" #())
		     #fn("6000r2|}P;" #())
		     #fn("8000s0c0|v2;" #(#.apply))
		     #fn("8000s0c0|v2;" #(#.+))
		     #fn("8000s0c0|v2;" #(#.-))
		     #fn("8000s0c0|v2;" #(#.*))
		     #fn("8000s0c0|v2;" #(#./))
		     #fn("8000s0c0|v2;" #(#.div0))
		     #fn("6000r2|}W;" #())
		     #fn("6000r2|}X;" #())
		     #fn("6000r2|}Y;" #())
		     #fn("8000s0c0|v2;" #(#.vector))
		     #fn("6000r2|}[;" #())
		     #fn("7000r3|}g2\\;" #()))
	*defer-macro-errors* #f *interactive* #f *keep-source* #f
	*print-closures* #t *print-shared* #t
	*syntax-environment* #table(letrec #fn(">000s1c0c0c1L1c2c3|32L1c2c4|32c5}3134L1c2c6|3242;" #(#fn(nconc)
  lambda #fn(map) #.car #fn("8000r1c0c1L1c2|3142;" #(#fn(nconc) set! #fn(copy-list)))
  #fn(copy-list) #fn("5000r1e040;" #(%fl:void))))  quasiquote #fn("7000r1e0|`42;" #(%fl:bq-process))  when #fn(";000s1c0|c1}K^L4;" #(if
  begin))  unwind-protect #fn("A000r2c030c030c1g7c2_}L3L2L1c3c4|c2g6L1c5g7L1c6g6L2L3L3L3g7L1L3L3;" #(#fn(gensym)
  let lambda prog1 trycatch begin raise))  dotimes #fn("A000s1|Me0|31c1`c2g7aL3c3c4L1g6L1L1c5}3133L4;" #(%fl:cadr
  for - #fn(nconc) lambda #fn(copy-list)))  define-macro #fn(">000s1c0c1|ML2c2c3L1|NL1c4}3133L3;" #(set-syntax!
  quote #fn(nconc) lambda #fn(copy-list)))  receive #fn("?000s2c0c1_}L3c2c1L1|L1c3g23133L3;" #(call-with-values
  lambda #fn(nconc) #fn(copy-list)))  unless #fn("<000s1c0|^c1}KL4;" #(if begin))  let* #fn("@000s1|?6E0c0c1L1_L1c2}3133L1;c0c1L1e3|31L1L1c2|NF6H0c0c4L1|NL1c2}3133L1530}3133e5|31L2;" #(#fn(nconc)
  lambda #fn(copy-list) %fl:caar let* %fl:cadar))  case #fn("A000s1c0Œ6g6c1OM2c230c3g7|L2L1c4c5L1c6c7g6g7q2}3232L3;" #(#{#<unspecified>}#
  #fn("8000r2}c0‚50c0;}…40^;}C6=0c1|e2}31L3;}?6=0c3|e2}31L3;}N…>0c3|e2}M31L3;e4c5}326=0c6|c7}L2L3;c8|c7}L2L3;" #(else
  eq? %fl:quote-value eqv? %fl:every #.symbol? memq quote memv) vals->cond)
  #fn(gensym) let #fn(nconc) cond #fn(map)
  #fn("7000r1~M\x7f|M32|NK;" #())))  catch #fn("@000r2c030c1}c2g6L1c3c4c5g6L2c6c7g6L2c8c9L2L3c6c:g6L2|L3L4c;g6L2c<g6L2L4L3L3;" #(#fn(gensym)
  trycatch lambda if and pair? eq car quote thrown-value cadr caddr raise))  assert #fn(";000r1c0|]c1c2c3|L2L2L2L4;" #(if
  raise quote assert-failed))  do #fn("J000s2c030}Mc1c2|32c1e3|32c1c4|32c5g7c6g9c7g8c8c9L1c:}N3132c8c9L1c:g231c8g7L1g;32L133L4L3L2L1c8g7L1g:32L3;" #(#fn(gensym)
  #fn(map) #.car %fl:cadr #fn("6000r1e0|31F680e1|41;|M;" #(%fl:cddr %fl:caddr))
  letrec lambda if #fn(nconc) begin #fn(copy-list)))  with-input-from #fn("<000s1c0c1L1c2|L2L1L1c3}3143;" #(#fn(nconc)
  with-bindings *input-stream* #fn(copy-list)))  let #fn(">000s1^|C6B0|m62}Mm02}Nm1540c02c1c2L1c3c4|32L1c5}3133c3c6|32g66C0c7g6g7L2L1g6L3540g7g8K;" #(#{#<unspecified>}#
  #fn(nconc) lambda #fn(map) #fn("5000r1|F650|M;|;" #())
  #fn(copy-list) #fn("6000r1|F680e0|41;e140;" #(%fl:cadr %fl:void)) letrec))  cond #fn(":000s0c0Œ5g5c1g5q1OM2g5M|41;" #(#{#<unspecified>}#
  #fn(">000r1|?640^;|Mg5Mc0<17802g5M]<6C0g5N…60g5M;c1g5NK;g5N…@0c2g5M~M|N31L3;e3g531c4‚˜0e5e6g531316c0e7e6g53131c8g6g5ML2L1c9g6c1e:e6g53131K~M|N31L4L3;c;30c8g6g5ML2L1c9g6e6g531g6L2~M|N31L4L3;c9g5Mc1g5NK~M|N31L4;" #(else
  begin or %fl:cadr => %fl:1arg-lambda? %fl:caddr %fl:caadr let if %fl:cddr #fn(gensym)) cond-clauses->if)))  throw #fn("9000r2c0c1c2c3L2|}L4L2;" #(raise
  list quote thrown-value))  time #fn(">000r1c030c1g5c2L1L2L1c3|c4c5c6c2L1g5L3c7L4L3L3;" #(#fn(gensym)
  let time.now prog1 princ "Elapsed time: " - " seconds\n"))  with-output-to #fn("<000s1c0c1L1c2|L2L1L1c3}3143;" #(#fn(nconc)
  with-bindings *output-stream* #fn(copy-list)))  with-bindings #fn("G000s1c0c1|32c0e2|32c0c3|32c4c5L1c0c6g8g633L1c7c0c8g6g73331c9c4c:L1c7}3132c4c:L1c7c0c;g6g8333132L3L144;" #(#fn(map)
  #.car %fl:cadr #fn("5000r1c040;" #(#fn(gensym)))
  #fn(nconc) let #.list #fn(copy-list) #fn("7000r2c0|}L3;" #(set!))
  unwind-protect begin #fn("7000r2c0|}L3;" #(set!)))))
	*whitespace* "\t\n\x0b\x0c\x0d Â…Â áš€á Žâ€€â€â€‚â€ƒâ€„â€…â€†â€‡â€ˆâ€‰â€Šâ€¨â€©â€¯âŸã€€"
	1+ #4# 1-
	#5# 1arg-lambda? #6# <=
	#7# > #8# >=
	#9# Instructions #table(not 16  vargc 67  load1 49  loadc0 78  = 39  sub2 72  brne.l 83  box 92  largc 74  brnn 85  loadc.l 58  loadi8 50  < 40  nop 0  set-cdr! 32  loada 55  bound? 21  / 37  removed-setc 63  neg 73  lvargc 75  brn.l 88  brt 7  trycatch 68  null? 17  load0 48  jmp.l 8  removed-setc.l 64  loadv 51  seta 61  keyargs 91  * 36  function? 26  builtin? 23  aref 43  optargs 89  vector? 24  loadt 45  brf 6  symbol? 19  cdr 30  for 69  cadr 84  pop 2  pair? 22  closure 65  loadf 46  compare 41  loadv.l 52  setg.l 60  brn 87  eqv? 13  aset! 44  eq? 12  atom? 15  boolean? 18  brt.l 10  tapply 70  dummy_nil 97  loada0 76  brbound 90  list 28  dup 1  apply 33  loadc 57  dummy_t 95  setg 59  loada1 77  tcall.l 81  jmp 5  fixnum? 25  cons 27  loadg.l 54  tcall 4  call 3  - 35  brf.l 9  + 34  shift 94  dummy_f 96  add2 71  seta.l 62  loadnil 47  brnn.l 86  set-car! 31  vector 42  loadg 53  box.l 93  loada.l 56  argc 66  loadc1 79  div0 38  ret 11  number? 20  equal? 14  car 29  call.l 80  brne 82)
	__init_globals #10# __script
	#11# __start #12# abs
	#13# any #14# arg-counts
	#table(#.equal? 2  #.atom? 1  #.set-cdr! 2  #.symbol? 1  #.car 1  #.eq? 2  #.aref 2  #.boolean? 1  #.not 1  #.null? 1  #.eqv? 2  #.number? 1  #.pair? 1  #.builtin? 1  #.aset! 3  #.div0 2  #.= 2  #.bound? 1  #.compare 2  #.vector? 1  #.cdr 1  #.set-car! 2  #.< 2  #.fixnum? 1  #.cons 2)
	argc-error #15# array?
	#16# assoc #17# assv
	#18# bcode:cenv #19# bcode:code
	#20# bcode:ctable #21# bcode:indexfor
	#22# bcode:nconst #23# bcode:sp
	#24# bcode:stack #25# box-vars
	#26# bq-bracket #27# bq-bracket1
	#28# bq-process #29#
	builtin->instruction #30# builtin-argc-ok?
	#31# caaaar #32# caaadr
	#33# caaar #34# caadar
	#35# caaddr #36# caadr
	#37# caar #38# cadaar
	#39# cadadr #40# cadar
	#41# caddar #42# cadddr
	#43# caddr #3# cadr
	#2# call-with-values #44# capture-var!
	#45# cdaaar #46# cdaadr
	#47# cdaar #48# cdadar
	#49# cdaddr #50# cdadr
	#51# cdar #52# cddaar
	#53# cddadr #54# cddar
	#55# cdddar #56# cddddr
	#57# cdddr #58# cddr
	#59# char? #60# closure?
	#61# compare<= #62# compile
	#63# compile-and #64# compile-app
	#65# compile-app* #66# compile-arglist
	#67# compile-begin #68#
	compile-builtin-call #69# compile-f
	#70# compile-f- #71# compile-for
	#72# compile-if #73# compile-in
	#74# compile-let #75# compile-or
	#76# compile-prog1 #77# compile-set!
	#78# compile-short-circuit #79# compile-sym
	#80# compile-thunk #81#
	compile-unknown-call #fn("5000r2^;" #() compile-unknown-call)
	compile-while #82# complex-bindings
	#83# complex-bindings- #84# const-to-idx-vec
	#85# copy-tree #86# count
	#87# define-head-name #88# delete-duplicates
	#89# disassemble #90# div
	#91# emit #92#
	emit-optional-arg-inits #93# encode-byte-code
	#94# env-names #95# error
	#96# eval #97# even?
	#98# every #99# expand
	#100# expand-define #101# expand-macro-call
	#102# extend-env #103# filter
	#104# fits-i8 #105# foldl
	#106# foldr #107# function-source
	#108# get-defined-vars #109# hex5
	#110# identity #111# in-env?
	#112# index-of #113# inlineable?
	#114# io.readall #115# io.readline
	#116# io.readlines #117# iota
	#118# keyword->symbol #119# keyword-arg?
	#120# lambda-arg-names #121# lambda-vars
	#122# lambda:body #123# lambda:defaults
	#124# lambda:source #125# lambda:vars
	#126# last-pair #127# lastcdr
	#128# length= #129# length>
	#130# list->vector #131# list-head
	#132# list-ref #133# list-tail
	#134# list? #135# load
	#136# load-process #137# lookup-sym
	#138# lower-arglist #139# lower-define
	#140# macrocall? #141# macroexpand-1
	#142# make-code-emitter #143# make-label
	#144# make-perfect-hash-table #145# make-system-image
	#146# map! #147# map-int
	#148# mark-label #149# max
	#150# member #151# memv
	#152# min #153# mod
	#154# mod0 #155# nan?
	#156# nary-compare #157# nary<
	#158# nary= #159# negative?
	#160# nestlist #161# newline
	#162# nreconc #163# odd?
	#164# positive? #165# princ
	#166# print #167# print-exception
	#168# print-stack-trace #169# print-to-string
	#170# printable? #171# proper-part
	#172# quote-value #173# quoted?
	#174# random #175# read-all
	#176# read-all-of #177# ref-int16-LE
	#178# ref-int32-LE #179# repl
	#180# resolve-global #fn("5000r1|;" #()) revappend
	#181# reverse #182# reverse!
	#183# reverse!- #184# reverse-
	#185# self-evaluating? #186# separate
	#187# set-syntax! #188# simple-compare
	#189# simple-sort #190# special-forms
	(quote if begin prog1 lambda and or while for return set! define
	 trycatch)
	splice-form? #191# string.join
	#192# string.lpad #193# string.map
	#194# string.rep #195# string.rpad
	#196# string.tail #197# string.trim
	#198# symbol-syntax #199# table.clone
	#200# table.foreach #201# table.invert
	#202# table.keys #203# table.pairs
	#204# table.values #205# to-proper
	#206# top-level-exception-handler
	#207# trace #208# traced?
	#209# uncurry-define #210# untrace
	#211# values #212# vars-diff
	#213# vars-to-env #214# vector->list
	#215# vector.map #216# vinfo
	#217# vinfo:heap? #2# vinfo:index
	#3# vinfo:sym #.car void
	#218# zero? #219#)
