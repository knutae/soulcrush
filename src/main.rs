use glsl::parser::Parse;
use glsl::syntax::ArraySpecifier;
use glsl::syntax::ArraySpecifierDimension;
use glsl::syntax::ArrayedIdentifier;
use glsl::syntax::Block;
use glsl::syntax::CaseLabel;
use glsl::syntax::Condition;
use glsl::syntax::Declaration;
use glsl::syntax::Expr;
use glsl::syntax::ExternalDeclaration;
use glsl::syntax::ForInitStatement;
use glsl::syntax::FullySpecifiedType;
use glsl::syntax::FunIdentifier;
use glsl::syntax::FunctionDefinition;
use glsl::syntax::FunctionParameterDeclaration;
use glsl::syntax::FunctionPrototype;
use glsl::syntax::InitDeclaratorList;
use glsl::syntax::Initializer;
use glsl::syntax::IterationStatement;
use glsl::syntax::JumpStatement;
use glsl::syntax::LayoutQualifierSpec;
use glsl::syntax::SelectionRestStatement;
use glsl::syntax::ShaderStage;
use glsl::syntax::SimpleStatement;
use glsl::syntax::Statement;
use glsl::syntax::TranslationUnit;
use glsl::syntax::TypeQualifier;
use glsl::syntax::TypeQualifierSpec;
use glsl::syntax::TypeSpecifier;
use glsl::syntax::TypeSpecifierNonArray;
use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file_contents = fs::read_to_string(filename).expect("Failed to read file");
    let stage = ShaderStage::parse(file_contents);
    let unit = stage.expect("Failed to parse glsl file");
    let functions = declared_functions(unit);
    println!("Declared functions: {functions:?}");
}

#[allow(dead_code)]
fn declared_functions(unit: TranslationUnit) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for decl in unit {
        match decl {
            ExternalDeclaration::FunctionDefinition(x) => names.push(x.prototype.name.to_string()),
            ExternalDeclaration::Declaration(_) => {}
            ExternalDeclaration::Preprocessor(_) => {}
        }
    }
    return names;
}

#[allow(dead_code)]
fn named_function<'a>(unit: &'a TranslationUnit, name: &str) -> Option<&'a FunctionDefinition> {
    for decl in unit {
        match decl {
            ExternalDeclaration::FunctionDefinition(x) => {
                if x.prototype.name.0 == name {
                    return Some(x);
                }
            }
            ExternalDeclaration::Declaration(_) => {}
            ExternalDeclaration::Preprocessor(_) => {}
        }
    }
    return None;
}

#[allow(dead_code)]
fn called_functions(func: &FunctionDefinition) -> HashSet<String> {
    struct State {
        functions: HashSet<String>,
    }
    impl State {
        fn new() -> Self {
            Self {
                functions: HashSet::new(),
            }
        }

        fn expr(&mut self, e: &Expr) {
            match e {
                Expr::Variable(_) => {}
                Expr::IntConst(_) => {}
                Expr::UIntConst(_) => {}
                Expr::BoolConst(_) => {}
                Expr::FloatConst(_) => {}
                Expr::DoubleConst(_) => {}
                Expr::Unary(_, e1) => self.expr(&e1),
                Expr::Binary(_, e1, e2) => {
                    self.expr(&e1);
                    self.expr(&e2);
                }
                Expr::Ternary(e1, e2, e3) => {
                    self.expr(&e1);
                    self.expr(&e2);
                    self.expr(&e3);
                }
                Expr::Assignment(lhs, _, rhs) => {
                    self.expr(&lhs);
                    self.expr(&rhs);
                }
                Expr::Bracket(e1, spec) => {
                    self.expr(&e1);
                    self.array_spec(&spec);
                }
                Expr::FunCall(id, params) => {
                    match id {
                        FunIdentifier::Identifier(x) => {
                            self.functions.insert(x.0.clone());
                        }
                        FunIdentifier::Expr(e1) => self.expr(&e1),
                    };
                    for e1 in params {
                        self.expr(e1);
                    }
                }
                Expr::Dot(e1, _) => self.expr(&e1),
                Expr::PostInc(e1) => self.expr(&e1),
                Expr::PostDec(e1) => self.expr(&e1),
                Expr::Comma(e1, e2) => {
                    self.expr(&e1);
                    self.expr(&e2);
                }
            }
        }

        fn initializer(&mut self, initializer: &Initializer) {
            match initializer {
                Initializer::Simple(e1) => self.expr(&e1),
                Initializer::List(list) => {
                    for e1 in list {
                        self.initializer(e1);
                    }
                }
            }
        }

        fn condition(&mut self, condition: &Condition) {
            match condition {
                Condition::Expr(e) => self.expr(&e),
                Condition::Assignment(ty, _, init) => {
                    self.fully_specified_type(ty);
                    self.initializer(init);
                }
            }
        }

        fn array_spec(&mut self, spec: &ArraySpecifier) {
            for dimension in &spec.dimensions {
                if let ArraySpecifierDimension::ExplicitlySized(e) = dimension {
                    self.expr(&e);
                }
            }
        }

        fn arrayed_identifier(&mut self, id: &ArrayedIdentifier) {
            if let Some(spec) = &id.array_spec {
                self.array_spec(&spec);
            }
        }

        fn type_specifier(&mut self, type_specifier: &TypeSpecifier) {
            if let TypeSpecifierNonArray::Struct(s) = &type_specifier.ty {
                for field in &s.fields {
                    self.type_specifier(&field.ty);
                    for id in &field.identifiers {
                        self.arrayed_identifier(&id);
                    }
                }
            }
        }

        fn fully_specified_type(&mut self, ty: &FullySpecifiedType) {
            if let Some(q) = &ty.qualifier {
                self.type_qualifier(&q);
            }
            self.type_specifier(&ty.ty);
        }

        fn type_qualifier(&mut self, type_qualifier: &TypeQualifier) {
            for qualifier in &type_qualifier.qualifiers {
                if let TypeQualifierSpec::Layout(layout) = qualifier {
                    for id in &layout.ids {
                        match id {
                            LayoutQualifierSpec::Identifier(_, e) => {
                                if let Some(e1) = e {
                                    self.expr(&e1);
                                }
                            }
                            LayoutQualifierSpec::Shared => {}
                        }
                    }
                }
            }
        }

        fn function_prototype(&mut self, prototype: &FunctionPrototype) {
            self.fully_specified_type(&prototype.ty);
            for parameter in &prototype.parameters {
                match parameter {
                    FunctionParameterDeclaration::Named(q, param) => {
                        if let Some(q) = q {
                            self.type_qualifier(&q);
                        }
                        self.arrayed_identifier(&param.ident);
                        self.type_specifier(&param.ty);
                    }
                    FunctionParameterDeclaration::Unnamed(q, ty) => {
                        if let Some(q) = q {
                            self.type_qualifier(&q);
                        }
                        self.type_specifier(&ty);
                    }
                }
            }
        }

        fn block(&mut self, block: &Block) {
            self.type_qualifier(&block.qualifier);
            for field in &block.fields {
                if let Some(qualifier) = &field.qualifier {
                    self.type_qualifier(&qualifier);
                }
                self.type_specifier(&field.ty);
                for id in &field.identifiers {
                    self.arrayed_identifier(&id);
                }
            }
            if let Some(id) = &block.identifier {
                self.arrayed_identifier(&id);
            }
        }

        fn init_declarator_list(&mut self, list: &InitDeclaratorList) {
            self.fully_specified_type(&list.head.ty);
            if let Some(array_spec) = &list.head.array_specifier {
                self.array_spec(&array_spec);
            }
            if let Some(init) = &list.head.initializer {
                self.initializer(&init);
            }
            for decl in &list.tail {
                self.arrayed_identifier(&decl.ident);
                if let Some(init) = &decl.initializer {
                    self.initializer(&init);
                }
            }
        }

        fn statement(&mut self, statement: &Statement) {
            match statement.clone() {
                Statement::Compound(compound) => {
                    for s1 in &compound.statement_list {
                        self.statement(&s1);
                    }
                }
                Statement::Simple(simple) => match *simple {
                    SimpleStatement::Declaration(decl) => match decl {
                        Declaration::FunctionPrototype(f) => self.function_prototype(&f),
                        Declaration::InitDeclaratorList(list) => self.init_declarator_list(&list),
                        Declaration::Precision(_, ty) => self.type_specifier(&ty),
                        Declaration::Block(block) => self.block(&block),
                        Declaration::Global(q, _) => self.type_qualifier(&q),
                    },
                    SimpleStatement::Expression(expr) => {
                        if let Some(expr) = expr {
                            self.expr(&expr);
                        }
                    }
                    SimpleStatement::Selection(selection) => {
                        self.expr(&selection.cond);
                        match selection.rest {
                            SelectionRestStatement::Statement(s1) => self.statement(&s1),
                            SelectionRestStatement::Else(s1, s2) => {
                                self.statement(&s1);
                                self.statement(&s2);
                            }
                        }
                    }
                    SimpleStatement::Switch(switch) => {
                        self.expr(&switch.head);
                        for s1 in switch.body {
                            self.statement(&s1);
                        }
                    }
                    SimpleStatement::CaseLabel(case) => match case {
                        CaseLabel::Case(e1) => self.expr(&e1),
                        CaseLabel::Def => {}
                    },
                    SimpleStatement::Iteration(it) => match it {
                        IterationStatement::While(condition, s1) => {
                            self.condition(&condition);
                            self.statement(&s1);
                        }
                        IterationStatement::DoWhile(s1, e1) => {
                            self.statement(&s1);
                            self.expr(&e1);
                        }
                        IterationStatement::For(init, rest, s1) => {
                            match init {
                                ForInitStatement::Expression(expr) => {
                                    if let Some(expr) = expr {
                                        self.expr(&expr);
                                    }
                                }
                                ForInitStatement::Declaration(decl) => match *decl {
                                    Declaration::FunctionPrototype(fp) => {
                                        self.function_prototype(&fp)
                                    }
                                    Declaration::InitDeclaratorList(list) => {
                                        self.init_declarator_list(&list)
                                    }
                                    Declaration::Precision(_, ty) => self.type_specifier(&ty),
                                    Declaration::Block(block) => self.block(&block),
                                    Declaration::Global(qualifier, _) => {
                                        self.type_qualifier(&qualifier)
                                    }
                                },
                            }
                            if let Some(condition) = rest.condition {
                                self.condition(&condition);
                            }
                            self.statement(&s1);
                        }
                    },
                    SimpleStatement::Jump(j) => {
                        if let JumpStatement::Return(expr) = j {
                            if let Some(expr) = expr {
                                self.expr(&expr);
                            }
                        }
                    }
                },
            }
        }
    }

    let mut state = State::new();
    state.function_prototype(&func.prototype);
    for statement in &func.statement.statement_list {
        state.statement(statement);
    }
    return state.functions;
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_declared_functions() {
        let glsl = "
            layout (location = 0) in vec3 pos;
            layout (location = 1) in vec4 col;

            out vec4 v_col;

            uniform mat4 projview;

            mat2 rotate(float degrees) {
                float r = degrees * PI / 180;
                return mat2(cos(r), -sin(r), sin(r), cos(r));
            }

            void main() {
                v_col = col; // pass color to the next stage
                gl_Position = projview * vec4(pos, 1.);
            }
        ";
        let stage = ShaderStage::parse(glsl);
        assert!(stage.is_ok());
        let unit = stage.unwrap();
        assert_eq!(declared_functions(unit), ["rotate", "main"]);
    }

    #[test]
    fn test_called_functions() {
        let glsl = "
            layout (location = 0) in vec3 pos;
            layout (location = 1) in vec4 col;

            out vec4 v_col;

            uniform mat4 projview;

            mat2 rotate(float degrees) {
                float r = degrees * PI / 180;
                return mat2(cos(r), -sin(r), sin(r), cos(r));
            }

            float func1() {
                return 42;
            }

            void func2(float angle) {
                gl_Position = projview * vec4(pos, 1.);
                gl_Position.xy *= rotate(angle);
            }

            float func3() {
                return func1();
            }

            void main() {
                if (gl_Position.x > 3) {
                    func2(func1());
                }
            }
        ";
        let stage = ShaderStage::parse(glsl);
        assert!(stage.is_ok());
        let unit = stage.unwrap();
        let main = named_function(&unit, "main").expect("missing function main");
        assert_eq!(
            called_functions(main),
            HashSet::from(["func1", "func2"].map(|x| x.to_string()))
        );
        let func1 = named_function(&unit, "func1").expect("missing function func1");
        assert_eq!(called_functions(func1), HashSet::new());
    }
}
