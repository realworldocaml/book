(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Detect CPU features.

    {e %%VERSION%% — {{: %%PKG_HOMEPAGE%%}homepage}} *)

(** {1 Overview}

    Cpuid provides runtime detection of CPU features through the x86 {b CPUID}
    instruction. Detection discovers the CPU {{!vendor}[vendor]} and a set of
    feature {{!flag}[flag]}s.

    The flag names reflect the Linux
    {{:https://git.kernel.org/cgit/linux/kernel/git/torvalds/linux.git/tree/arch/x86/include/asm/cpufeatures.h?id=c8d2bc9bc39ebea8437fd974fdbc21847bb897a3}convention},
    except in uppercase. Only the features reported by the CPU are included
    (i.e. there are no synthetic flags).

    {2:lim Limitations}

    Cpuid currently relies on an x86-specific feature. The library runs on ARM
    processors, but calls return an {{!error}[error]}.

    The only CPUID-leaves consulted for {{!flags}feature flags} are [0x1],
    [0x7:0] and [0x80000001]. Hence, the reported features are a subset
    of what Linux would report.

    Number of {{!cores}cores} is CPUID [0xb:1] [EBX]. This only works on Intel
    CPUs after around 2010, and fails under virtualization. *)

(** {1 Cpuid} *)

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
(** The CPU manufacturer, the type of virtual environment, or [`UNKNOWN]. *)

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
  | `MWAITX             (** MWAIT extension (MONITORX/MWAITX) *)
  (* Level 0x00000007 (EBX) *)
  | `FSGSBASE           (** \{RD/WR\}\{FS/GS\}BASE instructions*)
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
(** CPU flags signify presence of individual features.

    Consult the interface file for the meaning of individual flags. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] formats the {{!error}[error]} [e] to the formatter [ppf]. *)

val pp_vendor : Format.formatter -> vendor -> unit
(** [pp_vendor ppf v] formats the {{!vendor}[vendor]} [v] to the formatter [ppf]. *)

val pp_flag : Format.formatter -> flag -> unit
(** [pp_flag ppf f] formats the {{!flag}[flag]} [f] to the formatter [ppf]. *)

(** {2 Queries} *)

val vendor : unit -> vendor result
(** [vendor ()] is the CPU {!vendor}, or an [error]. *)

val model : unit -> (int * int * int) result
(** [model ()] is the CPU's {e family}, {e model} and {e stepping}, or an
    [error]. *)

val flags : unit -> flag list result
(** [flags ()] is the list of CPU {!flag}s, or an [error]. *)

val supports : flag list -> bool result
(** [supports fs] is [true] iff all [fs] are in [flags ()].*)

val cores : unit -> int result
(** [cores ()] is the number of available logical cores.

    {b Note} Do not take [cores ()] too seriously. See {{!lim}limitations}. *)
