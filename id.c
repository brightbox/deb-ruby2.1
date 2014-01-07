/* DO NOT EDIT THIS FILE DIRECTLY */
/**********************************************************************

  id.c -

  $Author: nobu $
  created at: Wed Dec  5 02:36:10 2012

  Copyright (C) 2004-2007 Koichi Sasada

**********************************************************************/

static void
Init_id(void)
{
#undef rb_intern
#define rb_intern(str) rb_intern_const(str)
    rb_encoding *enc = rb_usascii_encoding();

    REGISTER_SYMID(idFreeze, "freeze");
    REGISTER_SYMID(idInspect, "inspect");
    REGISTER_SYMID(idIntern, "intern");
    REGISTER_SYMID(idObject_id, "object_id");
    REGISTER_SYMID(idConst_missing, "const_missing");
    REGISTER_SYMID(idMethodMissing, "method_missing");
    REGISTER_SYMID(idMethod_added, "method_added");
    REGISTER_SYMID(idSingleton_method_added, "singleton_method_added");
    REGISTER_SYMID(idMethod_removed, "method_removed");
    REGISTER_SYMID(idSingleton_method_removed, "singleton_method_removed");
    REGISTER_SYMID(idMethod_undefined, "method_undefined");
    REGISTER_SYMID(idSingleton_method_undefined, "singleton_method_undefined");
    REGISTER_SYMID(idLength, "length");
    REGISTER_SYMID(idSize, "size");
    REGISTER_SYMID(idGets, "gets");
    REGISTER_SYMID(idSucc, "succ");
    REGISTER_SYMID(idEach, "each");
    REGISTER_SYMID(idProc, "proc");
    REGISTER_SYMID(idLambda, "lambda");
    REGISTER_SYMID(idSend, "send");
    REGISTER_SYMID(id__send__, "__send__");
    REGISTER_SYMID(id__attached__, "__attached__");
    REGISTER_SYMID(idInitialize, "initialize");
    REGISTER_SYMID(idInitialize_copy, "initialize_copy");
    REGISTER_SYMID(idInitialize_clone, "initialize_clone");
    REGISTER_SYMID(idInitialize_dup, "initialize_dup");
    REGISTER_SYMID(idUScore, "_");
    REGISTER_SYMID(idNULL, ""/*NULL*/"");
    REGISTER_SYMID(idEmptyP, "empty?");
    REGISTER_SYMID(idEqlP, "eql?");
    REGISTER_SYMID(idRespond_to, "respond_to?");
    REGISTER_SYMID(idRespond_to_missing, "respond_to_missing?");
    REGISTER_SYMID(idIFUNC, "<IFUNC>");
    REGISTER_SYMID(idCFUNC, "<CFUNC>");
    REGISTER_SYMID(id_core_set_method_alias, "core#set_method_alias");
    REGISTER_SYMID(id_core_set_variable_alias, "core#set_variable_alias");
    REGISTER_SYMID(id_core_undef_method, "core#undef_method");
    REGISTER_SYMID(id_core_define_method, "core#define_method");
    REGISTER_SYMID(id_core_define_singleton_method, "core#define_singleton_method");
    REGISTER_SYMID(id_core_set_postexe, "core#set_postexe");
    REGISTER_SYMID(id_core_hash_from_ary, "core#hash_from_ary");
    REGISTER_SYMID(id_core_hash_merge_ary, "core#hash_merge_ary");
    REGISTER_SYMID(id_core_hash_merge_ptr, "core#hash_merge_ptr");
    REGISTER_SYMID(id_core_hash_merge_kwd, "core#hash_merge_kwd");
}
