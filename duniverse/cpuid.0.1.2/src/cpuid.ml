(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type raw = {
  sign : int32;
  l1_eax : int32;
  l1_ecx : int32;
  l1_edx : int32;
  l7_ebx : int32;
  l7_ecx : int32;
  l12_ebx : int32;
  l1e_ecx : int32;
  l1e_edx : int32;
}

external cpudetect_raw : unit -> raw option = "caml_cpuid_cpudetect"

type error = [ `Unsupported ]

type nonrec 'a result = ('a, error) result

type vendor = [
  (* CPU manufacturer *)
    `AMD
  | `Centaur
  | `Cyrix
  | `Intel
  | `Transmeta
  | `NSC
  | `NexGen
  | `Rise
  | `SiS
  | `UMC
  | `VIA
  | `Vortex
  (* Virtual environment *)
  | `KVM
  | `Hyper_V
  | `Parallels
  | `VMware
  | `Xen
  (* WAT *)
  | `UNKNOWN
]

type flag = [
  (* Level 0x00000001 (EDX) *)
  | `FPU                (** Onboard FPU *)
  | `VME                (** Virtual Mode Extensions *)
  | `DE                 (** Debugging Extensions *)
  | `PSE                (** Page Size Extensions *)
  | `TSC                (** Time Stamp Counter *)
  | `MSR                (** Model-Specific Registers *)
  | `PAE                (** Physical Address Extensions *)
  | `MCE                (** Machine Check Exception *)
  | `CX8                (** CMPXCHG8 instruction *)
  | `APIC               (** Onboard APIC *)
  | `SEP                (** SYSENTER/SYSEXIT *)
  | `MTRR               (** Memory Type Range Registers *)
  | `PGE                (** Page Global Enable *)
  | `MCA                (** Machine Check Architecture *)
  | `CMOV               (** CMOV instructions *)
  | `PAT                (** Page Attribute Table *)
  | `PSE36              (** 36-bit PSEs *)
  | `PN                 (** Processor serial number *)
  | `CLFLUSH            (** CLFLUSH instruction *)
  | `DTS                (** Debug Store *)
  | `ACPI               (** ACPI via MSR *)
  | `MMX                (** Multimedia Extensions *)
  | `FXSR               (** FXSAVE/FXRSTOR, CR4.OSFXSR *)
  | `SSE                (** SSE *)
  | `SSE2               (** SSE2 *)
  | `SS                 (** CPU self snoop *)
  | `HT                 (** Hyper-Threading *)
  | `TM                 (** Automatic clock control *)
  | `IA64               (** IA-64 processor *)
  | `PBE                (** Pending Break Enable *)
  (* Level 0x80000001 (EDX) *)
  | `SYSCALL            (** SYSCALL/SYSRET *)
  | `MP                 (** MP Capable. *)
  | `NX                 (** Execute Disable *)
  | `MMXEXT             (** AMD MMX extensions *)
  | `FXSR_OPT           (** FXSAVE/FXRSTOR optimizations *)
  | `PDPE1GB            (** GB pages *)
  | `RDTSCP             (** RDTSCP *)
  | `LM                 (** Long Mode (x86-64) *)
  | `F_3DNOWEXT         (** AMD 3DNow! extensions *)
  | `F_3DNOW            (** 3DNow! *)
  (* Level 0x00000001 (EDX) *)
  | `PNI                (** SSE-3 *)
  | `PCLMULQDQ          (** PCLMULQDQ instruction *)
  | `DTES64             (** 64-bit Debug Store *)
  | `MONITOR            (** Monitor/Mwait support *)
  | `DS_CPL             (** CPL Qual. Debug Store *)
  | `VMX                (** Hardware virtualization *)
  | `SMX                (** Safer mode *)
  | `EST                (** Enhanced SpeedStep *)
  | `TM2                (** Thermal Monitor 2 *)
  | `SSSE3              (** Supplemental SSE-3 *)
  | `CID                (** Context ID *)
  | `SDBG               (** Silicon Debug *)
  | `FMA                (** Fused multiply-add *)
  | `CX16               (** CMPXCHG16B *)
  | `XTPR               (** Send Task Priority Messages *)
  | `PDCM               (** Performance Capabilities *)
  | `PCID               (** Process Context Identifiers *)
  | `DCA                (** Direct Cache Access *)
  | `SSE4_1             (** SSE-4.1 *)
  | `SSE4_2             (** SSE-4.2 *)
  | `X2APIC             (** x2APIC *)
  | `MOVBE              (** MOVBE instruction *)
  | `POPCNT             (** POPCNT instruction *)
  | `TSC_DEADLINE_TIMER (** Tsc deadline timer *)
  | `AES                (** AES instructions *)
  | `XSAVE              (** XSAVE/XRSTOR/XSETBV/XGETBV *)
  | `OSXSAVE            (** XSAVE enabled in the OS *)
  | `AVX                (** Advanced Vector Extensions *)
  | `F16C               (** 16-bit fp conversions *)
  | `RDRAND             (** The RDRAND instruction *)
  | `HYPERVISOR         (** Running on a hypervisor *)
  (* Level 0x80000001 (ECX) *)
  | `LAHF_LM            (** LAHF/SAHF in long mode *)
  | `CMP_LEGACY         (** If yes HyperThreading not valid *)
  | `SVM                (** Secure virtual machine *)
  | `EXTAPIC            (** Extended APIC space *)
  | `CR8_LEGACY         (** CR8 in 32-bit mode *)
  | `ABM                (** Advanced bit manipulation *)
  | `SSE4A              (** SSE-4A *)
  | `MISALIGNSSE        (** Misaligned SSE mode *)
  | `F_3DNOWPREFETCH    (** 3DNow prefetch instructions *)
  | `OSVW               (** OS Visible Workaround *)
  | `IBS                (** Instruction Based Sampling *)
  | `XOP                (** extended AVX instructions *)
  | `SKINIT             (** SKINIT/STGI instructions *)
  | `WDT                (** Watchdog timer *)
  | `LWP                (** Light Weight Profiling *)
  | `FMA4               (** 4 operands MAC instructions *)
  | `TCE                (** translation cache extension *)
  | `NODEID_MSR         (** NodeId MSR *)
  | `TBM                (** trailing bit manipulations *)
  | `TOPOEXT            (** topology extensions CPUID leafs *)
  | `PERFCTR_CORE       (** core performance counter extensions *)
  | `PERFCTR_NB         (** NB performance counter extensions *)
  | `BPEXT              (** data breakpoint extension *)
  | `PTSC               (** performance time-stamp counter *)
  | `PERFCTR_L2         (** L2 performance counter extensions *)
  | `MWAITX             (** MWAIT extension ( MONITORX/MWAITX) *)
  (* Level 0x00000007 (EBX) *)
  | `FSGSBASE           (** {RD/WR}{FS/GS}BASE instructions*)
  | `TSC_ADJUST         (** TSC adjustment MSR 0x3b *)
  | `BMI1               (** 1st group bit manipulation extensions *)
  | `HLE                (** Hardware Lock Elision *)
  | `AVX2               (** AVX2 instructions *)
  | `SMEP               (** Supervisor Mode Execution Protection *)
  | `BMI2               (** 2nd group bit manipulation extensions *)
  | `ERMS               (** Enhanced REP MOVSB/STOSB *)
  | `INVPCID            (** Invalidate Processor Context ID *)
  | `RTM                (** Restricted Transactional Memory *)
  | `CQM                (** Cache QoS Monitoring *)
  | `MPX                (** Memory Protection Extension *)
  | `AVX512F            (** AVX-512 Foundation *)
  | `AVX512DQ           (** AVX-512 DQ (Double/Quad granular) Instructions *)
  | `RDSEED             (** The RDSEED instruction *)
  | `ADX                (** The ADCX and ADOX instructions *)
  | `SMAP               (** Supervisor Mode Access Prevention *)
  | `CLFLUSHOPT         (** CLFLUSHOPT instruction *)
  | `CLWB               (** CLWB instruction *)
  | `AVX512PF           (** AVX-512 Prefetch *)
  | `AVX512ER           (** AVX-512 Exponential and Reciprocal *)
  | `AVX512CD           (** AVX-512 Conflict Detection *)
  | `SHA_NI             (** SHA1/SHA256 Instruction Extensions *)
  | `AVX512BW           (** AVX-512 BW (Byte/Word granular) Instructions *)
  | `AVX512VL           (** AVX-512 VL (128/256 Vector Length) Extensions *)
  (* Level 0x00000007 (ECX) *)
  | `PKU                (** Protection Keys for Userspace *)
  | `OSPKE              (** OS Protection Keys Enable *)
]

module FlagS = Set.Make (struct
  type t = flag
  let compare (a : flag) b = compare a b
end)

type id = {
  vendor : vendor;
  model  : int * int * int;
  flags  : FlagS.t;
  cores  : int;
}

let pp_error fmt e =
  Format.pp_print_string fmt @@
  match e with `Unsupported -> "Unsupported"

let pp_vendor fmt v =
  Format.pp_print_string fmt @@
  match v with
  | `AMD       -> "AMD"
  | `Centaur   -> "Centaur"
  | `Cyrix     -> "Cyrix"
  | `Intel     -> "Intel"
  | `Transmeta -> "Transmeta"
  | `NSC       -> "NSC"
  | `NexGen    -> "NexGen"
  | `Rise      -> "Rise"
  | `SiS       -> "SiS"
  | `UMC       -> "UMC"
  | `VIA       -> "VIA"
  | `Vortex    -> "Vortex"
  | `KVM       -> "KVM"
  | `Hyper_V   -> "Hyper_V"
  | `Parallels -> "Parallels"
  | `VMware    -> "VMware"
  | `Xen       -> "Xen"
  | `UNKNOWN   -> "UNKNOWN"

let pp_flag fmt f =
  Format.pp_print_string fmt @@
  match f with
  | `FPU                -> "FPU"
  | `VME                -> "VME"
  | `DE                 -> "DE"
  | `PSE                -> "PSE"
  | `TSC                -> "TSC"
  | `MSR                -> "MSR"
  | `PAE                -> "PAE"
  | `MCE                -> "MCE"
  | `CX8                -> "CX8"
  | `APIC               -> "APIC"
  | `SEP                -> "SEP"
  | `MTRR               -> "MTRR"
  | `PGE                -> "PGE"
  | `MCA                -> "MCA"
  | `CMOV               -> "CMOV"
  | `PAT                -> "PAT"
  | `PSE36              -> "PSE36"
  | `PN                 -> "PN"
  | `CLFLUSH            -> "CLFLUSH"
  | `DTS                -> "DTS"
  | `ACPI               -> "ACPI"
  | `MMX                -> "MMX"
  | `FXSR               -> "FXSR"
  | `SSE                -> "SSE"
  | `SSE2               -> "SSE2"
  | `SS                 -> "SS"
  | `HT                 -> "HT"
  | `TM                 -> "TM"
  | `IA64               -> "IA64"
  | `PBE                -> "PBE"
  | `SYSCALL            -> "SYSCALL"
  | `MP                 -> "MP"
  | `NX                 -> "NX"
  | `MMXEXT             -> "MMXEXT"
  | `FXSR_OPT           -> "FXSR_OPT"
  | `PDPE1GB            -> "PDPE1GB"
  | `RDTSCP             -> "RDTSCP"
  | `LM                 -> "LM"
  | `F_3DNOWEXT         -> "3DNOWEXT"
  | `F_3DNOW            -> "3DNOW"
  | `PNI                -> "PNI"
  | `PCLMULQDQ          -> "PCLMULQDQ"
  | `DTES64             -> "DTES64"
  | `MONITOR            -> "MONITOR"
  | `DS_CPL             -> "DS_CPL"
  | `VMX                -> "VMX"
  | `SMX                -> "SMX"
  | `EST                -> "EST"
  | `TM2                -> "TM2"
  | `SSSE3              -> "SSSE3"
  | `CID                -> "CID"
  | `SDBG               -> "SDBG"
  | `FMA                -> "FMA"
  | `CX16               -> "CX16"
  | `XTPR               -> "XTPR"
  | `PDCM               -> "PDCM"
  | `PCID               -> "PCID"
  | `DCA                -> "DCA"
  | `SSE4_1             -> "SSE4_1"
  | `SSE4_2             -> "SSE4_2"
  | `X2APIC             -> "X2APIC"
  | `MOVBE              -> "MOVBE"
  | `POPCNT             -> "POPCNT"
  | `TSC_DEADLINE_TIMER -> "TSC_DEADLINE_TIMER"
  | `AES                -> "AES"
  | `XSAVE              -> "XSAVE"
  | `OSXSAVE            -> "OSXSAVE"
  | `AVX                -> "AVX"
  | `F16C               -> "F16C"
  | `RDRAND             -> "RDRAND"
  | `HYPERVISOR         -> "HYPERVISOR"
  | `LAHF_LM            -> "LAHF_LM"
  | `CMP_LEGACY         -> "CMP_LEGACY"
  | `SVM                -> "SVM"
  | `EXTAPIC            -> "EXTAPIC"
  | `CR8_LEGACY         -> "CR8_LEGACY"
  | `ABM                -> "ABM"
  | `SSE4A              -> "SSE4A"
  | `MISALIGNSSE        -> "MISALIGNSSE"
  | `F_3DNOWPREFETCH    -> "3DNOWPREFETCH"
  | `OSVW               -> "OSVW"
  | `IBS                -> "IBS"
  | `XOP                -> "XOP"
  | `SKINIT             -> "SKINIT"
  | `WDT                -> "WDT"
  | `LWP                -> "LWP"
  | `FMA4               -> "FMA4"
  | `TCE                -> "TCE"
  | `NODEID_MSR         -> "NODEID_MSR"
  | `TBM                -> "TBM"
  | `TOPOEXT            -> "TOPOEXT"
  | `PERFCTR_CORE       -> "PERFCTR_CORE"
  | `PERFCTR_NB         -> "PERFCTR_NB"
  | `BPEXT              -> "BPEXT"
  | `PTSC               -> "PTSC"
  | `PERFCTR_L2         -> "PERFCTR_L2"
  | `MWAITX             -> "MWAITX"
  | `FSGSBASE           -> "FSGSBASE"
  | `TSC_ADJUST         -> "TSC_ADJUST"
  | `BMI1               -> "BMI1"
  | `HLE                -> "HLE"
  | `AVX2               -> "AVX2"
  | `SMEP               -> "SMEP"
  | `BMI2               -> "BMI2"
  | `ERMS               -> "ERMS"
  | `INVPCID            -> "INVPCID"
  | `RTM                -> "RTM"
  | `CQM                -> "CQM"
  | `MPX                -> "MPX"
  | `AVX512F            -> "AVX512F"
  | `AVX512DQ           -> "AVX512DQ"
  | `RDSEED             -> "RDSEED"
  | `ADX                -> "ADX"
  | `SMAP               -> "SMAP"
  | `CLFLUSHOPT         -> "CLFLUSHOPT"
  | `CLWB               -> "CLWB"
  | `AVX512PF           -> "AVX512PF"
  | `AVX512ER           -> "AVX512ER"
  | `AVX512CD           -> "AVX512CD"
  | `SHA_NI             -> "SHA_NI"
  | `AVX512BW           -> "AVX512BW"
  | `AVX512VL           -> "AVX512VL"
  | `PKU                -> "PKU"
  | `OSPKE              -> "OSPKE"

let vendor_of_sig_ebx = function
  | 0x68747541l -> `AMD
  | 0x746e6543l -> `Centaur
  | 0x69727943l -> `Cyrix
  | 0x756e6547l -> `Intel
  | 0x6e617254l -> `Transmeta
  (* | 0x756e6547 -> `Transmeta2 *)
  | 0x646f6547l -> `NSC
  | 0x4778654el -> `NexGen
  | 0x65736952l -> `Rise
  | 0x20536953l -> `SiS
  | 0x20434d55l -> `UMC
  | 0x20414956l -> `VIA
  | 0x74726f56l -> `Vortex
  | 0x4b4d564bl -> `KVM
  | 0x7263694dl -> `Hyper_V
  | 0x70726c20l -> `Parallels
  | 0x61774d56l -> `VMware
  | 0x566e6558l -> `Xen
  | _           -> `UNKNOWN

let l1_edx_f = function
  | 0  -> `FPU
  | 1  -> `VME
  | 2  -> `DE
  | 3  -> `PSE
  | 4  -> `TSC
  | 5  -> `MSR
  | 6  -> `PAE
  | 7  -> `MCE
  | 8  -> `CX8
  | 9  -> `APIC
  | 11 -> `SEP
  | 12 -> `MTRR
  | 13 -> `PGE
  | 14 -> `MCA
  | 15 -> `CMOV
  | 16 -> `PAT
  | 17 -> `PSE36
  | 18 -> `PN
  | 19 -> `CLFLUSH
  | 21 -> `DTS
  | 22 -> `ACPI
  | 23 -> `MMX
  | 24 -> `FXSR
  | 25 -> `SSE
  | 26 -> `SSE2
  | 27 -> `SS
  | 28 -> `HT
  | 29 -> `TM
  | 30 -> `IA64
  | 31 -> `PBE
  | _  -> `Nothing

let l1e_edx_f = function
  | 11 -> `SYSCALL
  | 19 -> `MP
  | 20 -> `NX
  | 22 -> `MMXEXT
  | 25 -> `FXSR_OPT
  | 26 -> `PDPE1GB
  | 27 -> `RDTSCP
  | 29 -> `LM
  | 30 -> `F_3DNOWEXT
  | 31 -> `F_3DNOW
  | _  -> `Nothing

let l1_ecx_f = function
  | 0  -> `PNI
  | 1  -> `PCLMULQDQ
  | 2  -> `DTES64
  | 3  -> `MONITOR
  | 4  -> `DS_CPL
  | 5  -> `VMX
  | 6  -> `SMX
  | 7  -> `EST
  | 8  -> `TM2
  | 9  -> `SSSE3
  | 10 -> `CID
  | 11 -> `SDBG
  | 12 -> `FMA
  | 13 -> `CX16
  | 14 -> `XTPR
  | 15 -> `PDCM
  | 17 -> `PCID
  | 18 -> `DCA
  | 19 -> `SSE4_1
  | 20 -> `SSE4_2
  | 21 -> `X2APIC
  | 22 -> `MOVBE
  | 23 -> `POPCNT
  | 24 -> `TSC_DEADLINE_TIMER
  | 25 -> `AES
  | 26 -> `XSAVE
  | 27 -> `OSXSAVE
  | 28 -> `AVX
  | 29 -> `F16C
  | 30 -> `RDRAND
  | 31 -> `HYPERVISOR
  | _  -> `Nothing

let l1e_ecx_f = function
  | 0  -> `LAHF_LM
  | 1  -> `CMP_LEGACY
  | 2  -> `SVM
  | 3  -> `EXTAPIC
  | 4  -> `CR8_LEGACY
  | 5  -> `ABM
  | 6  -> `SSE4A
  | 7  -> `MISALIGNSSE
  | 8  -> `F_3DNOWPREFETCH
  | 9  -> `OSVW
  | 10 -> `IBS
  | 11 -> `XOP
  | 12 -> `SKINIT
  | 13 -> `WDT
  | 15 -> `LWP
  | 16 -> `FMA4
  | 17 -> `TCE
  | 19 -> `NODEID_MSR
  | 21 -> `TBM
  | 22 -> `TOPOEXT
  | 23 -> `PERFCTR_CORE
  | 24 -> `PERFCTR_NB
  | 26 -> `BPEXT
  | 27 -> `PTSC
  | 28 -> `PERFCTR_L2
  | 29 -> `MWAITX
  | _  -> `Nothing

let l7_ebx_f = function
  | 0  -> `FSGSBASE
  | 1  -> `TSC_ADJUST
  | 3  -> `BMI1
  | 4  -> `HLE
  | 5  -> `AVX2
  | 7  -> `SMEP
  | 8  -> `BMI2
  | 9  -> `ERMS
  | 10 -> `INVPCID
  | 11 -> `RTM
  | 12 -> `CQM
  | 14 -> `MPX
  | 16 -> `AVX512F
  | 17 -> `AVX512DQ
  | 18 -> `RDSEED
  | 19 -> `ADX
  | 20 -> `SMAP
  | 23 -> `CLFLUSHOPT
  | 24 -> `CLWB
  | 26 -> `AVX512PF
  | 27 -> `AVX512ER
  | 28 -> `AVX512CD
  | 29 -> `SHA_NI
  | 30 -> `AVX512BW
  | 31 -> `AVX512VL
  | _  -> `Nothing

let l7_ecx_f = function
  | 3 -> `PKU
  | 4 -> `OSPKE
  | _ -> `Nothing

let bit x i = Int32.(shift_right_logical x i |> to_int) land 1 = 1
let fl f reg s =
  let rec go s = function
    | 32 -> s
    | i when bit reg i ->
        go (match f i with #flag as f -> FlagS.add f s | `Nothing -> s) (succ i)
    | i -> go s (succ i) in
  go s 0

let fms eax =
  let r = Int32.to_int eax in
  let family =
    match (r lsr 8) land 0xf with
    | 0xf -> 0xf + ((r lsr 20) land 0xff)
    | x   -> x in
  let model =
    (r lsr 4) land 0xf +
      if family >= 6 then ((r lsr 16) land 0xf) lsl 4 else 0
  and stepping = r land 0xf in
  (family, model, stepping)

let cores _ reg = Int32.to_int reg
(* This works with Intel after 2010, status unknown otherwise.
   God's own official dox at
   https://stackoverflow.com/questions/24088837/logical-cpu-count-return-16-instead-of-4/24704156#24704156 *)
(* match vendor with | `Intel -> Int32.to_int reg | _ -> -1 *)

let id () =
  match cpudetect_raw () with
  | None -> None
  | Some raw ->
      let vendor = vendor_of_sig_ebx raw.sign
      and flags =
        fl l1_edx_f raw.l1_edx @@ fl l1e_edx_f raw.l1e_edx @@
        fl l1_ecx_f raw.l1_ecx @@ fl l1e_ecx_f raw.l1e_ecx @@
        fl l7_ebx_f raw.l7_ebx @@ fl l7_ecx_f raw.l7_ecx FlagS.empty
      and model = fms raw.l1_eax in
      let cores = cores vendor raw.l12_ebx in
      Some { vendor; model; flags; cores }

let _id = lazy (id ())
let q f = match Lazy.force _id with
  Some id -> Ok (f id) | _ -> Error `Unsupported

let vendor () = q @@ fun id -> id.vendor
let model () = q @@ fun id -> id.model
let flags () = q @@ fun id -> FlagS.elements id.flags
let supports qfs = q @@ fun id ->
  List.for_all (fun qf -> FlagS.mem qf id.flags) qfs
let cores () = q @@ fun id -> id.cores
