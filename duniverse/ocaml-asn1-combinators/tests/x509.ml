(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn
open Asn.S

type tBSCertificate = {
  version    : [ `V1 | `V2 | `V3 ] ;
  serial     : Z.t ;
  signature  : OID.t ;
  issuer     : (OID.t * string) list list ;
  validity   : Ptime.t * Ptime.t ;
  subject    : (OID.t * string) list list ;
  pk_info    : OID.t * Cstruct.t ;
  issuer_id  : Cstruct.t option ;
  subject_id : Cstruct.t option ;
  extensions : (OID.t * bool * Cstruct.t) list option
}

type certificate = {
  tbs_cert       : tBSCertificate ;
  signature_algo : OID.t ;
  signature      : Cstruct.t
}

let def  x = function None -> x | Some y -> y

let def' x = fun y -> if y = x then None else Some y

let extensions =
  let extension =
    map (fun (oid, b, v) -> (oid, def  false b, v))
        (fun (oid, b, v) -> (oid, def' false b, v)) @@
    sequence3
      (required ~label:"id"       oid)
      (optional ~label:"critical" bool) (* default false *)
      (required ~label:"value"    octet_string)
  in
  sequence_of extension

let directory_name =
  map (function | `C1 s -> s | `C2 s -> s | `C3 s -> s
                | `C4 s -> s | `C5 s -> s | `C6 s -> s)
      (function s -> `C1 s)
  @@
  choice6
    printable_string utf8_string
    (* The following three could probably be ommited.
      * See rfc5280 section 4.1.2.4. *)
    teletex_string universal_string bmp_string
    (* is this standard? *)
    ia5_string

let name =
  let attribute_tv =
   sequence2
      (required ~label:"attr type"  oid)
      (* This is ANY according to rfc5280. *)
      (required ~label:"attr value" directory_name) in
  let rd_name      = set_of attribute_tv in
  let rdn_sequence = sequence_of rd_name in
  rdn_sequence (* A vacuous choice, in the standard. *)

let algorithmIdentifier =
  map (fun (oid, _) -> oid) (fun oid -> (oid, None))
  @@
  sequence2
    (required ~label:"algorithm" oid)
    (* This is ANY according to rfc5280 *)
    (optional ~label:"params"    null)

let version =
  map (function 2 -> `V2 | 3 -> `V3 | _ -> `V1)
      (function `V2 -> 2 | `V3 -> 3 | _ -> 1)
  int

let certificateSerialNumber = integer

let time =
  map (function `C1 t -> t | `C2 t -> t) (fun t -> `C2 t)
      (choice2 utc_time generalized_time)

let validity =
  sequence2
    (required ~label:"not before" time)
    (required ~label:"not after"  time)

let subjectPublicKeyInfo =
  sequence2
    (required ~label:"algorithm" algorithmIdentifier)
    (required ~label:"subjectPK" bit_string_cs)

let uniqueIdentifier = bit_string_cs

let tBSCertificate =
  let f = fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) ->
    { version    = def `V1 a ; serial     = b ;
      signature  = c         ; issuer     = d ;
      validity   = e         ; subject    = f ;
      pk_info    = g         ; issuer_id  = h ;
      subject_id = i         ; extensions = j }

  and g = fun
    { version    = a ; serial     = b ;
      signature  = c ; issuer     = d ;
      validity   = e ; subject    = f ;
      pk_info    = g ; issuer_id  = h ;
      subject_id = i ; extensions = j } ->
    (def' `V1 a, (b, (c, (d, (e, (f, (g, (h, (i, j)))))))))
  in

  map f g @@
  sequence @@
      (optional ~label:"version"       @@ explicit 0 version) (* default v1 *)
    @ (required ~label:"serialNumber"  @@ certificateSerialNumber)
    @ (required ~label:"signature"     @@ algorithmIdentifier)
    @ (required ~label:"issuer"        @@ name)
    @ (required ~label:"validity"      @@ validity)
    @ (required ~label:"subject"       @@ name)
    @ (required ~label:"subjectPKInfo" @@ subjectPublicKeyInfo)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"issuerUID"     @@ implicit 1 uniqueIdentifier)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"subjectUID"    @@ implicit 2 uniqueIdentifier)
      (* v3 if present *)
   -@ (optional ~label:"extensions"    @@ explicit 3 extensions)

let certificate =

  let f (a, b, c) =
    { tbs_cert = a ; signature_algo = b ; signature = c }

  and g { tbs_cert = a ; signature_algo = b ; signature = c } =
    (a, b, c) in

  map f g @@
  sequence3
    (required ~label:"tbsCertificate"     tBSCertificate)
    (required ~label:"signatureAlgorithm" algorithmIdentifier)
    (required ~label:"signatureValue"     bit_string_cs)

let cert_ber, cert_der =
  (codec ber certificate, codec der certificate)


let examples = [
  "3082062030820408a003020102 0203020acd300d06092a864886
   f70d0101050500305431143012 060355040a130b434163657274
   20496e632e311e301c06035504 0b1315687474703a2f2f777777
   2e4341636572742e6f7267311c 301a0603550403131343416365
   727420436c617373203320526f 6f74301e170d31333039303431
   35343333395a170d3135303930 343135343333395a3069310b30
   09060355040613024445311030 0e0603550408130748616d6275
   72673110300e06035504071307 48616d627572673121301f0603
   55040a13184368616f7320436f 6d707574657220436c75622065
   2e562e31133011060355040313 0a7777772e6363632e64653082
   0222300d06092a864886f70d01 010105000382020f003082020a
   0282020100b867422b9bec4548 8639b5e86d7b8848beec017328
   59f6e96a2e63846237169fa933 61a5294bb9bffb936f64195ba0
   c75a81d6d900207c447baa466d b5aa2ddb758207388b1f74499e
   c070bbc09b5076c2c8d7296362 0416d7e6aa47abac84aee26b17
   596853a90cb27c5a57f066f6ed 3b29d878e9a1e7e3199d953fe0
   bd364b24af59e9ff87c4ded1f3 6598747680d95ccfb52090e7cf
   517cf450edd8364259837019bc c336eaafebb4f5fed770981c05
   88fa44de4d425f78bc12bcfab8 cade0a8a2d09d83401490dc143
   bd813c6054c5b009e98d382544 f948a9fe77403e134274cea8ba
   851e5037b00a3a075226676aea acc9064e5987c1b96534af2e91
   96442462a40a02b66aabe22a9d b1b6af1babec2c5556fc53725e
   1659f4a810a7a4b1b2deca5bef 1cd986f10a0f3945768f708f5d
   cd0ce3974f9ea0c27961b14304 28de8fefabd4f21658c2d02d60
   037360cef1b9939ae33be90671 4094a1cc4c0d3edac2426dfdc3
   5af18e79ea94fec4e014755809 05313ff6e22482f9b0eb3f30fb
   f0d9df5954e3bb44095a559f39 ef054ed3452e17aa2fc877c683
   2cde17dc56f5306f125f839307 d2f01d706efacd691bd131ff89
   f5ec89d7502cb5c7dfc1d15530 c32e7df56eddfe341d264c8291
   75957f729f4175b44af54da20d 60bdadde11ce0c0a970713b9e1
   9886432a9471a188c58ca5fb45 5b1e821b57d900a34b99020301
   0001a381e53081e2300c060355 1d130101ff04023000300e0603
   551d0f0101ff0404030203a830 340603551d25042d302b06082b
   0601050507030206082b060105 0507030106096086480186f842
   0401060a2b0601040182370a03 03303306082b06010505070101
   04273025302306082b06010505 0730018617687474703a2f2f6f
   6373702e6361636572742e6f72 672f30380603551d1f0431302f
   302da02ba0298627687474703a 2f2f63726c2e6361636572742e
   6f72672f636c617373332d7265 766f6b652e63726c301d060355
   1d1104163014820a7777772e63 63632e646582066363632e6465
   300d06092a864886f70d010105 0500038202010085e6700b89e3
   7917899556187be487bed8e5ce 563eaaf527c01e8808cf3dbd8a
   24b8a7a83d075a460e4e97df44 261f3a9c3986d5903bda265a31
   e152b7fef5cb951605770417d8 c53d6b238592bc166523ea43c4
   2689bfc02e36e66320999d2807 13117e7209f03668c2d84f610b
   0c1e0d53281fa03bbe46b5c378 e3a4f7ecf46b4e2414be366f71
   5881b37f9a0e9262e48dd8fc4b 18bbfb710418f9564ffa692419
   ab9d2a029c5895a48f46ad7120 3cdfbf47739308b1b6336fe579
   4bf1f5bdecc141ddc22e9052af 87427dbebb2e3b16372d771f4a
   cdeec993d1e7081437ae67f9d3 f8ebfab96d15571337323e16be
   879c65b34278314e75e54222cc 089bdbb3fe3dc7ef793c7e994d
   acb58712cc97504d6fc2740758 3f1853f9bd635c2d792fa63fc0
   0a20e4083a57106b3ed90d39a4 a9d4e3680fab5bc0917afc11bc
   85114d189d893a82184f5832c9 72af0d67911c6c5917f77e1c54
   a3fe7cb8d1b675e9d327711f9d 21eb321652ea11397525e74fa3
   3b6cb24480f180a0ae3a7e9897 d73b9834bda98d4cdcf226bc77
   2f94c9603db78aed0eb23b4cf0 d80a9b35953b09e9b9235f42ab
   b4ee46e35400405a403b4bedf6 4c0a884400e83c314eaba4f031
   1dae70b25e33d0475661b8acee 57425f8d39858fa9bafb5443fa
   030e303bee0843be66e2f46957 68d5a7f5114de83c8c7e0b543b
   d905b092eec7229685bf4ffe";

  "308204763082035ea003020102 020843b5eedccc2793ee300d06
   092a864886f70d010105050030 49310b30090603550406130255
   5331133011060355040a130a47 6f6f676c6520496e6331253023
   0603550403131c476f6f676c65 20496e7465726e657420417574
   686f72697479204732301e170d 3134303132393134303533375a
   170d3134303532393030303030 305a3068310b30090603550406
   13025553311330110603550408 0c0a43616c69666f726e696131
   16301406035504070c0d4d6f75 6e7461696e2056696577311330
   11060355040a0c0a476f6f676c 6520496e633117301506035504
   030c0e7777772e676f6f676c65 2e636f6d30820122300d06092a
   864886f70d0101010500038201 0f003082010a0282010100a478
   79a679863bb8c311c4a835e0d3 f1f3316d0ff566508d9be05750
   6200fc02e4627c0f9faafc6270 4922ed37754ab678ce57670236
   c04be7c2d1e4238bc7e8253a2c ae45e0420bf976cd3ef2553776
   8a155e8a9e99e24a52287323f8 7eedc7f5dbceffec46cc23945a
   0c150f4c79991de0ed937f1751 8b01ad2f779c80aae150d4031c
   b604ab06492da5f7046f9787e1 7430e682e4397110ca9ffa6a75
   812a02ac455448da9b08dc5164 81b1696a4a7dfb7c8f6cfcc643
   0b37ccc33e8085e14cad134bd2 8276637715741c620d576a8c64
   be006e6a214cff02cbc734bdc9 12c6b9e4e4ab305b9b08f0b360
   330054b2b38aa657e46db97347 bfaa1d1b48ae3f0203010001a3
   8201413082013d301d0603551d 250416301406082b0601050507
   030106082b0601050507030230 190603551d1104123010820e77
   77772e676f6f676c652e636f6d 306806082b0601050507010104
   5c305a302b06082b0601050507 3002861f687474703a2f2f706b
   692e676f6f676c652e636f6d2f 47494147322e637274302b0608
   2b06010505073001861f687474 703a2f2f636c69656e7473312e
   676f6f676c652e636f6d2f6f63 7370301d0603551d0e04160414
   7520ead1f9b9b734d5e9e4358a aee864c6732ba4300c0603551d
   130101ff04023000301f060355 1d230418301680144add06161b
   bcf668b576f581b6bb621aba5a 812f30170603551d200410300e
   300c060a2b06010401d6790205 0130300603551d1f0429302730
   25a023a021861f687474703a2f 2f706b692e676f6f676c652e63
   6f6d2f47494147322e63726c30 0d06092a864886f70d01010505
   0003820101003a8fda0f284e64 fc55f9b1b2d8e29ef1b2796d9d
   d1c3375a32ce66fcf9c9a47ba5 bf7851ec63483ecd4794056df3
   6f410c06735758d4c207569521 c4467bc1940c30270334973100
   5e062b0d6faf649f6ba7b52ed1 6e52fcdfef07efced1b0b797b9
   c6a1af7902a1ceb5a137a62341 c4238dce0ed548b851033490c4
   d70aac1e475979c9cd4b6f4867 24a92b6b24af7ac7eea5246cfd
   659336c5bec9c5532a770094b8 89bf7ee313ebeb91907d48bff2
   f828495bcecb9637ad3fd4dc2b 48f6d3e80d26536064e5eb82c3
   c496bc744198993287823c891e 66cacdeb35dcdfc1375f17525b
   d39e311a89f417bc98fdca9a9c 3075053e392ac08d474b26f589
   1b61";

  "30820263308201cc020900cb6c 4e844b58a1d4300d06092a8648
   86f70d01010505003076310b30 09060355040613024155311330
   1106035504080c0a536f6d652d 53746174653121301f06035504
   0a0c18496e7465726e65742057 69646769747320507479204c74
   643115301306035504030c0c59 4f5552204e414d452121213118
   301606092a864886f70d010901 16096d65406261722e6465301e
   170d3134303231373232303834 355a170d313530323137323230
   3834355a3076310b3009060355 04061302415531133011060355
   04080c0a536f6d652d53746174 653121301f060355040a0c1849
   6e7465726e6574205769646769 747320507479204c7464311530
   1306035504030c0c594f555220 4e414d45212121311830160609
   2a864886f70d01090116096d65 406261722e646530819f300d06
   092a864886f70d010101050003 818d0030818902818100b64048
   dee6bc21943da2ab5eb6f8d837 007f417c0fe33492c3aa2f553e
   4d5e31434689c26f2be68e00d2 88b0e3abf6fe118845d9498985
   12f192cbe49fd5b0831f01cb2d 274db3a638f5befb3ce81ab6b5
   59393444044fedd6ca154f76bf bd525608bb550a39bbd2ed12e6
   d71f9f84ba21aa5e2180150267 1aab049af8640da10203010001
   300d06092a864886f70d010105 0500038181008a38669a48969d
   c947296d442d7f032082d2db21 e5374cdd6ef6e7cc1da0fde511
   ed3c5252f0a673dc689fdc5fca cc1b85dfe22b7bef2adb56b537
   32e9811063794d6e239f8fa267 215ba7a4d3dce505e799ec5c38
   cd1c16ee75e0d5a46b8f4c8e82 650561539a84305df19a5a241b
   e555f870834e094d41cf9f74b3 342e8345";
]
