"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Enhancing screen/dynpro by docking contain</p>
class ZCX_CA_SCR_ENHANCEMENT_BY_DC definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    BEGIN OF zcx_ca_enh_screen_by_dc,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '081',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_enh_screen_by_dc .
  constants:
    BEGIN OF no_proper_environment,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '082',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_proper_environment .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_SCR_ENHANCEMENT_BY_DC type SEOCLSNAME value 'ZCX_CA_SCR_ENHANCEMENT_BY_DC' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_SCR_ENHANCEMENT_BY_DC IMPLEMENTATION.


  method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_ENH_SCREEN_BY_DC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
