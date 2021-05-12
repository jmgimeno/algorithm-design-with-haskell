module Chapter4 where

type Nat = Integer -- to avoid precission errors

-- ONE-DIMENSIONAL SEARCH PROBLEM

linearSearch :: (Nat -> Nat) -> Nat -> [Nat]
linearSearch f t = seek (0, t)
  where
    seek (a, b) = [x | x <- [a .. b], t == f x]

search1D :: (Nat -> Nat) -> Nat -> [Nat]
search1D f t = seek (0, t)
  where
    seek (a, b)
      | a > b = []
      | t < f m = seek (a, m - 1)
      | t == f m = [m]
      | otherwise = seek (m + 1, b)
      where
        m = choose (a, b)
    choose (a, b) = (a + b) `div` 2

bound :: (Nat -> Nat) -> Nat -> (Nat, Nat)
bound f t = if t <= f 0 then (-1, 0) else (b `div` 2, b)
  where
    b = until done (* 2) 1
    done b = t <= f b

search1D' :: (Nat -> Nat) -> Nat -> [Nat]
search1D' f t = [x | x == t]
  where
    x = smallest (bound f t) f t

smallest :: (Nat, Nat) -> (Nat -> Nat) -> Nat -> Nat
smallest (a, b) f t
  | a + 1 == b = b
  | t <= f m = smallest (a, m) f t
  | otherwise = smallest (m, b) f t
  where
    m = (a + b) `div` 2

-- TWO-DIMENSIONAL SEARCH PROBLEM

quadraticSearch :: ((Nat, Nat) -> Nat) -> Nat -> [(Nat, Nat)]
quadraticSearch f t = [(x, y) | x <- [0 .. t], y <- [0 .. t], t == f (x, y)]

quadraticSearch' :: ((Nat, Nat) -> Nat) -> Nat -> [(Nat, Nat)]
quadraticSearch' f t = [(x, y) | x <- [0 .. t], y <- [t, t -1 .. 0], t == f (x, y)]

--searchIn :: (Nat, Nat) -> ((Nat, Nat) -> Nat) -> Nat -> [(Nat, Nat)]
--searchIn (a, b) f t = [(x, y) | x <- [a .. t], y <- [b, b -1 .. 0], t == f (x, y)]

search2D :: ((Nat, Nat) -> Nat) -> Nat -> [(Nat, Nat)]
search2D f t = searchIn (0, t)
  where
    searchIn (x, y)
      | x > t || y < 0 = []
      | z < t = searchIn (x + 1, y)
      | z == t = (x, y) : searchIn (x + 1, y - 1)
      | z > t = searchIn (x, y - 1)
      where
        z = f (x, y)

f :: (Nat, Nat) -> Nat
f (x, y) = x ^ 2 + 3 ^ y

-- >>> f (24,9)
-- 20259

-- >>> f (0, 20259)
-- 998847910953408858969736819274020584470423236383453862147364148074145447846328941901494632340376765791196219588131673080259553659625241117458202748543033281011423865618228708229276999144297747739239654150103677330150037719169544484078902707845122729252613355293015182667902711845752834841690102903736347110907540392162262436417191260306079898241004961776726926447908590095341182836843432477551960755477641989120657138472203134717584699427763619206380542138265429262506261869047435517049093694448424288081700589218436089797871062040972353050841324153100864130230288069897232955521837448743123659788526382689038981905768855361032880321503797235758200421272603784020026475858892220105260628344698327564765132063245758692452459530175159308032267496170977375601571747863063645600730328372202019167987079875649814851260778349576440861837723884538204901907917437285842402224952370254788941865758499026212632989652091568703852118891746423913858881976318665354692371038756233598694846954192217241696192775906268314244904608480979340799160006024929336369940529572708552698312784298626413955654183567847306498301612603359491262369934230880948065137202002342747682610945015146456014707859730493529498043840609786777193715530751538570787490463493271331196063916295029729577011678359316157997960297039574498433097766465374734550252968722430179096695633933977663312828088637576623953213098606505702392331477233014980280651483868396986258915915636416500339041162615959131750156372357717548679647881913848992359492276234520739170097613270768076750155786774248302916124138765312424342700902704972810367963113413234551068646086340434147780725538313910018533828863340908187204614985602876473131821571284715303354483801143305690627348668243897956218402678958437252514282282191731276172490849936245514351572063839576577384735521654702576980758601065581999405615645277550059230884477343680743933202123827616649682648510832194744004477350570868026946960040877441631916888327328265003294116179753871445675792008940572109064162845210365414318426046996343731339050334355130545020989566950697837721908786334129356140667439355479440413484811530696080376516968716298451543945634290330621689320924177110090562325939674090506779899193058006684807044770177909986736997197070083389673359360973443939851889195514481321553901122008033940138018558778160158499672267003923136270714817221602424726086214439441341616619622514748773772724557363035509601125066057384883306723662005979350741656060863065703003843910585259958721069180986742922920883415557336569787414914946204621202576156308150013534007770132228401664035647497435952932757309279558602252564858041612498878651679772874174457558832685460440184758067375638023031214390248796591601318895100994650753019240650435034523218102640599720089401950495194002570119741035996579434398749683404202638050389412160371731121692603615033135771379990983961544831733353036423801674171025845962796490773003219211196218137763130574500344776178877973045070142079019948857519498951118796712336075357317021952852131858540101770907433649437035628815876261172870695245769363737731633397799408801146240176968463510471878347699298729923984141513183804350257879024920666733882517827021668271766822512149402277226672344294463638108877427300301666714841938132228061835229260932519810713806023668753571526564110071928370660603793208043355606102752258617194321998389897386635152708545059131739483898922935854994466398666341196910158573054098781406252715295122432358902828455305689667918229779143780521965080088854795233424714560549782550470149947298956337503889618394537169474475388412171560159141290968808688121751525373738562372571282833306549502396534585439342163487331124792295098878153389656545461560468244331195293915599302821832181077315728668699013165239274322458954060934211496166112051006089097041593721591694902181150610175590881336502997321584506219032694597772708175918918627427007851957645994851713939033158550885757342791612583076459136600556265390946598408640204819765171788391668650768246228132910582114965994582982610310637319354209038492179601289801441891623109099823703632560376791257771778880877443639423521687657134337912566028088094290230689049938951177699170153050976359167434055225225695886101298140772160321954857990366238849821459671122119453545608251284376837131973581795632971335727144288445059150612389356964058631374337344955772586791344351622660225815669428088900201085639705932831033907998782296686336032427524173313775465802749656860727437457367540568671247100838320144236367278005552212212594792881082259919444929696556004362483342368295533176269034193514235610378694814747340393503122372183693027320205024242320346299899578547259233816779897143407809142068006775066244303403676061819302591748326753237498441696218468785320931211860307878419332735423913836714621985685944073864019263852536736369553702142913867262753622174465308994585830186954778091921249378546061156443011748594565500992327900487054695742801105515894816561007798519450624214297697096106581271828485874055533749497760834050008695002444088276498753689322629700931324335863887112336545908693753645235898916571907138902818750919920475612630829646584180084770962311703361099549420323647282749627319587368835509757897331693818944414518000219381068128410430765957971737683622633085269431744683778582703003984709875689936312249815192703555929442773571384097142310779011156525120532161076907058501601321034403047073141884376723952546163652119241193388963146655300856403278819297386308402469548378451852170184756212919386524254460530314901899653467879735779816866276321666232341472510274693941619762477513683590013894729000890359122372447965214246014875381883748646396729654853482566367945306014718958571357891314100055438271235957884989908176781495716837726355626533490784818799376419418376485374676302829807156905724566085707395291986518973775275919692778064450947328285440457851763890835501086542694796900629435628205164373565526874026964680241868650783070245570448063860123393630739453172052769448118661178065317618224847171492232458779805419685379111321048238709020021933829961289018278989193045931905051949274329802523196375967948657377149894485797419261668822044937832441420640501307948500906609946799185742845240454474340656530009946361241416026018463947380132805129735652805570992913353727759654528223628208235595541061309675400998622819179328299034296057783440591880472043808132693931579256607289218022578809926689649097167287740674055251564536888438832010616403998379402769981770788805557556080574810983301051674270433273784188230275030950803435138858957535558641962251448248486142254039002278185040190598990499086019783999573494932327030477063168263990313653293493968975077063633114063385674126621352095237925866105120099795381014388601531556916331497325480433953669699121632145441483699343546298761230928757966719044310306278861021942776965935485882059093948694714235624445959541107550616126948220419590938278814153833920074621462987658140839103802629954434972497245739401722812959382064523684692773113854150806571398315828701522601593299196041776597829206320293784625542817875679243173997771654683335030587760337169430642717508925506590839683149593145040415614015342948324813419669370647337987751694719749493544617683863006841105609418436704781519750715329847868039121074942040042625092471773602715845712829751456860439495379083567901441276806275384561727903829501095060999648634850270626275136506357185832381804182188220907345766219464090588815464126369446754904654380722789051664126196701736382821613963103653622996938729787448614027920788572211185675092398435920648232638246453882823837642033749309651232898980190227126057665150235676110319580815661128359383321363484580737013175780389275881032937439869916915837113057248345513257519396164772247025301148443839725293681608071141636217924475806029339754086489571183421197697256941551760561163832822258208110293403509549964397030206238281937655887326133913436779452571603891280217754599508271489062652575759077626366329867870549159624645015686708016395705570500867542750403258109250013341806267766700602765880245475785659155076702191183491894801168335496184787720332568515322135375929818126105858217392753659156330951831925190151627161885039505732464301991027652269323318534875031103723956841917951570327538853835146235345977332772030081714884462494851146362543648061945417812388258113533834627989435581254201230816984144614668425448702973922676389531331111775266381845291723354896754770965351861791987323711101745434063121055189991047390096704416996175949129869056635891936899420274865036669571041766081111651815427406140451508738652140132400258854670802164688553206327426230733168909063472606637593712010095576929989573386759277405994536152771653392600461111607687018057294385248486609543682035163607097216881402452648353713112707481880876273431899623597263637527435916245446174000234946615220681549401689119979238080749222275914278918467870963783149911983986486890092785409905493292551419294566763509958276141185893388611437469035985353398477504473564154742740813422352610547065657412648261659158390697109664988319370486674853571683877359170512862228721464131073007153561310913308198324718450848553404213069396411796301542901037713758481149406505570041305623807982795502085574994000271008645404863895029805528158907155407580091542730676770193813201890777274735885305448256090772473549167618631645793969825720898907593051764249461769864298902527019115170208622365745224313342640266246869356062138063182337901250012545808624045282672179381895078986682765632419380641485345420933078971838401709459824675308006348529963983987362434977108882797905700963571152193314105446202928755607280349486952275197695421212617879559067

-- >>>
-- >>> search2D (\(x, y) -> x^2 + 3^y) 20259
-- [(24,9)]

search2D' :: ((Nat, Nat) -> Nat) -> Nat -> [(Nat, Nat)]
search2D' f t = from (0, p) (q, 0)
  where
    p = smallest (-1, t) (\y -> f (0, y)) t
    q = smallest (-1, t) (\x -> f (x, 0)) t
    from (x1, y1) (x2, y2)
      | x2 < x1 || y1 < y2 = []
      | y1 - y2 <= x2 - x1 = row x
      | otherwise = col y
      where
        x = smallest (x1 - 1, x2) (\x -> f (x, r)) t
        y = smallest (y2 - 1, y1) (\y -> f (c, y)) t
        c = (x1 + x2) `div` 2
        r = (y1 + y2) `div` 2
        row x
          | z < t = from (x1, y1) (x2, r + 1)
          | z == t = (x, r) : from (x1, y1) (x - 1, r + 1) ++ from (x + 1, r - 1) (x2, y2)
          | z > t = from (x1, y1) (x - 1, r + 1) ++ from (x, r - 1) (x2, y2)
          where
            z = f (x, r)
        col y
          | z < t = from (c + 1, y1) (x2, y2)
          | x == t = (c, y) : from (x1, y1) (c - 1, y + 1) ++ from (c + 1, y - 1) (x2, y2)
          | z > t = from (x1, y1) (c - 1, y) ++ from (c + 1, y - 1) (x2, y2)
          where
            z = f (c, y)

-- >>> search2D (\(x, y) -> x^2 + 3^y) 20259
-- [(24,9)]
