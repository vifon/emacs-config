;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
                     '(("vec" "std::vector<${1:Class}> ${2:var}${3:(${4:10}, $1($5))};" "vector" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/vector" nil nil)
                       ("using" "using namespace ${std};\n$0" "using namespace ..." nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/using" nil nil)
                       ("up" "std::unique_ptr<${1:T}> ${2:ptr}(new $1($3));" "unique_ptr" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/unique_ptr" "direct-keybinding" nil)
                       ("tryw" "try {\n    `(or yas/selected-text (car kill-ring))`\n} catch ${1:Exception} {\n\n}\n" "tryw" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/tryw" nil nil)
                       ("try" "try {\n    $0\n} catch (const ${1:std::exception}& ${2:e}) {\n    std::cout << $2.what() << std::endl;\n}" "try { ...} catch ( ... ) { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/try" nil nil)
                       ("throw" "throw ${1:MyError}($0);" "throw" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/throw" nil nil)
                       ("th" "this" "this" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/this" nil nil)
                       ("ts" "BOOST_AUTO_TEST_SUITE( ${1:test_suite1} )\n\n$0\n\nBOOST_AUTO_TEST_SUITE_END()" "test_suite" nil
                        ("testing")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/test_suite" nil nil)
                       ("test_main" "int main(int argc, char **argv) {\n      ::testing::InitGoogleTest(&argc, argv);\n       return RUN_ALL_TESTS();\n}" "test_main" nil
                        ("testing")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/test_main" nil nil)
                       ("tc" "BOOST_AUTO_TEST_CASE( ${1:test_case} )\n{\n        $0\n}" "test case" nil
                        ("testing")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/test case" nil nil)
                       ("temp" "template<${1:$$(yas/choose-value '(\"typename\" \"class\"))} ${2:T}>\n$0" "template" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/template_" nil nil)
                       ("template" "template <typename ${T}>" "template <typename ...>" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/template" nil nil)
                       ("str" "#include <string>" "str" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/str" nil nil)
                       ("st" "std::$0" "std::" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/std_colon" nil nil)
                       ("std" "using namespace std;" "std" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/std" nil nil)
                       ("ss" "#include <sstream>" "<sstream>" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/sstream" nil nil)
                       ("say" "std::cout << $1 << std::endl;" "std::cout << ... << std::endl;" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/say" nil nil)
                       ("pb" "public:\n        $0" "public" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/public" nil nil)
                       ("pt" "protected:\n        $0" "protected" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/protected" nil nil)
                       ("pr" "private:\n        $0" "private" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/private" nil nil)
                       ("pack" "void cNetCommBuffer::pack(${1:type}) {\n\n}\n\n$0" "pack" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/pack" nil nil)
                       ("os" "#include <ostream>" "ostream" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/ostream" nil nil)
                       ("<<" "std::ostream& operator<<(std::ostream& s, const ${1:type}& ${2:c})\n{\n         $0\n         return s;\n}" "operator<<" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator_ostream" nil nil)
                       (">>" "istream& operator>>(istream& s, const ${1:type}& ${2:c})\n{\n         $0\n}\n" "operator>>" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator_istream" nil nil)
                       ("[]" "${1:Type}& operator[](${2:int index})\n{\n        $0\n}" "operator[]" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator[]" nil nil)
                       ("==" "bool ${1:MyClass}::operator==(const $1 &other) const {\n     $0\n}" "operator==" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator==" nil nil)
                       ("=" "${1:MyClass}& $1::operator=(const $1 &rhs) {\n    // Check for self-assignment!\n    if (this == &rhs)\n      return *this;\n    $0\n    return *this;\n}" "operator=" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator=" nil nil)
                       ("+=" "${1:MyClass}& $1::operator+=(${2:const $1 &rhs})\n{\n  $0\n  return *this;\n}" "operator+=" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator+=" nil nil)
                       ("+" "${1:MyClass} $1::operator+(const $1 &other)\n{\n    $1 result = *this;\n    result += other;\n    return result;\n}" "operator+" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator+" nil nil)
                       ("!=" "bool ${1:MyClass}::operator!=(const $1 &other) const {\n    return !(*this == other);\n}" "operator!=" nil
                        ("operator overloading")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/operator!=" nil nil)
                       ("ns" "namespace ${1:Namespace} {\n          \n          `yas/selected-text`\n\n}" "namespace" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/ns" nil nil)
                       ("namespace" "namespace $1 {\n$0\n} // namespace $1" "namespace ..." nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/namespace" nil nil)
                       ("mod" "class ${1:Class} : public cSimpleModule\n{\n   $0\n}" "module" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/module" nil nil)
                       ("map" "std::map<${1:type1}$0> ${2:var};" "map" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/map" nil nil)
                       ("list" "std::list<${1:T}>$0;" "std::list<...>;" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/list" nil nil)
                       ("iter" "${1:std::}${2:vector<int>}::iterator ${3:iter};\n" "iterator" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/iterator" nil nil)
                       ("io" "#include <iostream>" "io" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/io" nil nil)
                       ("il" "inline $0" "inline" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/inline" nil nil)
                       ("infloop" "bool _ = true;\nfor (;_;) {\n    $0\n}" "while (1)" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/infloop" nil nil)
                       ("ignore" "${1:std::}cin.ignore(std::numeric_limits<std::streamsize>::max(), '\\n');" "ignore" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/ignore" nil nil)
                       ("gtest" "#include <gtest/gtest.h>" "gtest" nil
                        ("testing")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/gtest" nil nil)
                       ("functor" "class ${1:Functor} {\n  public:\n    ${2:void} operator()(${3:...})\n    {\n        $0\n    }\n};" "functor" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/functor" "direct-keybinding" nil)
                       ("f" "${1:type} ${2:Class}::${3:name}(${4:args})${5: const}\n{\n        $0\n}" "function" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/function" nil nil)
                       ("f" "${1:type} ${2:name}(${3:args})${4: const};" "fun_declaration" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/fun_declaration" nil nil)
                       ("fr" "friend $0;" "friend" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/friend" nil nil)
                       ("fori" "for (${1:size_t} ${2:i} = ${3:0}; $2 < ${4:N}; ++$2) {\n    $0\n}" "for ( size_t ...; ...; ...) { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/fori" nil nil)
                       ("foreee" "for (${1:auto }${2:it} = ${3:var}.begin(); $2 != $3.end(); ++$2) {\n    $0\n}" "foreee" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/foreee" nil nil)
                       ("foree" "for (${1:std::vector<${2:T}>}::${3:iterator} ${4:it} = ${5:container}.$6begin();\n     $4 != $5.$6end();\n     ++$4) {\n\n    $0\n}" "for ( iterator ...; ...; ...) { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/foree" nil nil)
                       ("fore" "for (auto ${1:i} : ${2:container}) {\n    $0\n}" "for ( auto& ... : ... ) { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/fore" nil nil)
                       ("fixt" "BOOST_FIXTURE_TEST_SUITE( ${1:name}, ${2:Fixture} )\n\n$0\n\nBOOST_AUTO_TEST_SUITE_END()" "fixture" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/fixture" nil nil)
                       ("exception" "class ${1:Name} : public std::exception\n{\n  public:\n    const char* what() const throw()\n    {\n        return \"${2:message}\";\n    }\n};" "exception ... { ... }" nil nil
                        ((yas/indent-line 'fixed))
                        "/home/vifon/.emacs.d/snippets/c++-mode/exception" nil nil)
                       ("enum" "enum ${1:NAME}{\n$0\n};" "enum" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/enum" nil nil)
                       ("cast" "check_and_cast<${1:Type} *>(${2:msg});" "dynamic_casting" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/dynamic_casting" nil nil)
                       ("deque" "std::deque<${1:T}>$0;" "std::deque<...>;" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/deque" nil nil)
                       ("dla" "delete[] ${1:arr};" "delete[]" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/delete[]" nil nil)
                       ("dl" "delete ${1:pointer};" "delete" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/delete" nil nil)
                       ("c[" "const ${1:Type}& operator[](${2:int index}) const;" "d_operator[]_const" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/d_operator[]_const" nil nil)
                       ("[" "${1:Type}& operator[](${2:int index});" "d_operator[]" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/d_operator[]" nil nil)
                       ("<<" "friend std::ostream& operator<<(std::ostream&, const ${1:Class}&);" "d_operator<<" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/d_operator" nil nil)
                       ("d+=" "${1:MyClass}& operator+=(${2:const $1 &});" "d+=" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/d+=" nil nil)
                       ("cstd" "#include <cstdlib>" "cstd" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/cstd" nil nil)
                       ("cpp" "#include \"`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`.h\"" "cpp" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/cpp" nil nil)
                       ("cout" "std::cout << ${1:string} $0<< std::endl;" "cout" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/cout" nil nil)
                       ("ct" "${1:Class}::$1(${2:args})${3:\n    : ${4:init}}\n{\n        $0\n}" "constructor" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/constructor" nil nil)
                       ("c[" "const ${1:Type}& operator[](${2:int index}) const\n{\n        $0\n}" "const_[]" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/const_[]" nil nil)
                       ("coll" "${1:std::${2:vector}}<${3:T}> ${3:$(\nlet ((text (downcase\n            (replace-regexp-in-string \".*[:<] *\\\\\\\\(.*?\\\\\\\\)[> ]*?$\"\n                                      \"\\\\\\\\1\"\n                                      (or yas-text \"\")))))\n  (if (string-match \"[s|x]$\" text)\n      (concat text \"es\")\n      (concat text \"s\")))}$0" "collection<ns::Object> objects;" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/coll" nil nil)
                       ("classtest" "class ${1:Name}\n{\npublic:\n    $2\n    ${1:$(yas/substr yas-text \"[^: ]*\")}(){}\n    virtual ~${1:$(yas/substr yas-text \"[^: ]*\")}(){}\n};" "class ... { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/classtest" nil nil)
                       ("cls" "class ${1:Name}\n{\npublic:\n    ${1:$(yas/substr yas-text \"[^: ]*\")}();\n    ${2:virtual ~${1:$(yas/substr yas-text \"[^: ]*\")}();}\n};\n$0" "class" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/class" nil nil)
                       ("cin" "cin >> $0;" "cin" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/cin" nil nil)
                       ("err" "cerr << $0;\n" "cerr" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/cerr" nil nil)
                       ("req" "BOOST_REQUIRE( ${1:condition} );\n$0" "boost_require" nil
                        ("boost")
                        nil "/home/vifon/.emacs.d/snippets/c++-mode/boost_require" nil nil)
                       ("beginend" "${1:v}.begin(), $1.end()" "v.begin(), v.end()" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/beginend" nil nil)
                       ("ass" "assert($0);" "assert" nil nil nil "/home/vifon/.emacs.d/snippets/c++-mode/assert" nil nil)))


;;; Do not edit! File generated at Thu Jan 23 18:55:15 2020
