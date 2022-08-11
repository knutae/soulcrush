use glsl::parser::Parse as _;
use glsl::syntax;
use glsl::transpiler::glsl::show_translation_unit;
use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file_contents = fs::read_to_string(filename).expect("Failed to read file");
    let stage = syntax::ShaderStage::parse(file_contents);
    let unit = stage.expect("Failed to parse glsl file");
    let modified_unit = remove_unused_functions(&unit);
    let mut output = String::new();
    show_translation_unit(&mut output, &modified_unit);
    println!("{output}");
}

fn declared_functions(unit: &syntax::TranslationUnit) -> Vec<String> {
    let mut names: Vec<String> = Vec::new();
    for decl in unit {
        match decl {
            syntax::ExternalDeclaration::FunctionDefinition(x) => {
                names.push(x.prototype.name.to_string())
            }
            syntax::ExternalDeclaration::Declaration(_) => {}
            syntax::ExternalDeclaration::Preprocessor(_) => {}
        }
    }
    return names;
}

fn named_function<'a>(
    unit: &'a syntax::TranslationUnit,
    name: &str,
) -> Option<&'a syntax::FunctionDefinition> {
    for decl in unit {
        match decl {
            syntax::ExternalDeclaration::FunctionDefinition(x) => {
                if x.prototype.name.0 == name {
                    return Some(x);
                }
            }
            syntax::ExternalDeclaration::Declaration(_) => {}
            syntax::ExternalDeclaration::Preprocessor(_) => {}
        }
    }
    return None;
}

fn called_functions(func: &syntax::FunctionDefinition) -> HashSet<String> {
    struct State {
        functions: HashSet<String>,
    }
    impl State {
        fn new() -> Self {
            Self {
                functions: HashSet::new(),
            }
        }

        fn expr(&mut self, e: &syntax::Expr) {
            match e {
                syntax::Expr::Variable(_) => {}
                syntax::Expr::IntConst(_) => {}
                syntax::Expr::UIntConst(_) => {}
                syntax::Expr::BoolConst(_) => {}
                syntax::Expr::FloatConst(_) => {}
                syntax::Expr::DoubleConst(_) => {}
                syntax::Expr::Unary(_, e1) => self.expr(&e1),
                syntax::Expr::Binary(_, e1, e2) => {
                    self.expr(&e1);
                    self.expr(&e2);
                }
                syntax::Expr::Ternary(e1, e2, e3) => {
                    self.expr(&e1);
                    self.expr(&e2);
                    self.expr(&e3);
                }
                syntax::Expr::Assignment(lhs, _, rhs) => {
                    self.expr(&lhs);
                    self.expr(&rhs);
                }
                syntax::Expr::Bracket(e1, spec) => {
                    self.expr(&e1);
                    self.array_spec(&spec);
                }
                syntax::Expr::FunCall(id, params) => {
                    match id {
                        syntax::FunIdentifier::Identifier(x) => {
                            self.functions.insert(x.0.clone());
                        }
                        syntax::FunIdentifier::Expr(e1) => self.expr(&e1),
                    };
                    for e1 in params {
                        self.expr(e1);
                    }
                }
                syntax::Expr::Dot(e1, _) => self.expr(&e1),
                syntax::Expr::PostInc(e1) => self.expr(&e1),
                syntax::Expr::PostDec(e1) => self.expr(&e1),
                syntax::Expr::Comma(e1, e2) => {
                    self.expr(&e1);
                    self.expr(&e2);
                }
            }
        }

        fn initializer(&mut self, initializer: &syntax::Initializer) {
            match initializer {
                syntax::Initializer::Simple(e1) => self.expr(&e1),
                syntax::Initializer::List(list) => {
                    for e1 in list {
                        self.initializer(e1);
                    }
                }
            }
        }

        fn condition(&mut self, condition: &syntax::Condition) {
            match condition {
                syntax::Condition::Expr(e) => self.expr(&e),
                syntax::Condition::Assignment(ty, _, init) => {
                    self.fully_specified_type(ty);
                    self.initializer(init);
                }
            }
        }

        fn array_spec(&mut self, spec: &syntax::ArraySpecifier) {
            for dimension in &spec.dimensions {
                if let syntax::ArraySpecifierDimension::ExplicitlySized(e) = dimension {
                    self.expr(&e);
                }
            }
        }

        fn arrayed_identifier(&mut self, id: &syntax::ArrayedIdentifier) {
            if let Some(spec) = &id.array_spec {
                self.array_spec(&spec);
            }
        }

        fn type_specifier(&mut self, type_specifier: &syntax::TypeSpecifier) {
            if let syntax::TypeSpecifierNonArray::Struct(s) = &type_specifier.ty {
                for field in &s.fields {
                    self.type_specifier(&field.ty);
                    for id in &field.identifiers {
                        self.arrayed_identifier(&id);
                    }
                }
            }
        }

        fn fully_specified_type(&mut self, ty: &syntax::FullySpecifiedType) {
            if let Some(q) = &ty.qualifier {
                self.type_qualifier(&q);
            }
            self.type_specifier(&ty.ty);
        }

        fn type_qualifier(&mut self, type_qualifier: &syntax::TypeQualifier) {
            for qualifier in &type_qualifier.qualifiers {
                if let syntax::TypeQualifierSpec::Layout(layout) = qualifier {
                    for id in &layout.ids {
                        match id {
                            syntax::LayoutQualifierSpec::Identifier(_, e) => {
                                if let Some(e1) = e {
                                    self.expr(&e1);
                                }
                            }
                            syntax::LayoutQualifierSpec::Shared => {}
                        }
                    }
                }
            }
        }

        fn function_prototype(&mut self, prototype: &syntax::FunctionPrototype) {
            self.fully_specified_type(&prototype.ty);
            for parameter in &prototype.parameters {
                match parameter {
                    syntax::FunctionParameterDeclaration::Named(q, param) => {
                        if let Some(q) = q {
                            self.type_qualifier(&q);
                        }
                        self.arrayed_identifier(&param.ident);
                        self.type_specifier(&param.ty);
                    }
                    syntax::FunctionParameterDeclaration::Unnamed(q, ty) => {
                        if let Some(q) = q {
                            self.type_qualifier(&q);
                        }
                        self.type_specifier(&ty);
                    }
                }
            }
        }

        fn block(&mut self, block: &syntax::Block) {
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

        fn init_declarator_list(&mut self, list: &syntax::InitDeclaratorList) {
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

        fn statement(&mut self, statement: &syntax::Statement) {
            match statement.clone() {
                syntax::Statement::Compound(compound) => {
                    for s1 in &compound.statement_list {
                        self.statement(&s1);
                    }
                }
                syntax::Statement::Simple(simple) => match *simple {
                    syntax::SimpleStatement::Declaration(decl) => match decl {
                        syntax::Declaration::FunctionPrototype(f) => self.function_prototype(&f),
                        syntax::Declaration::InitDeclaratorList(list) => {
                            self.init_declarator_list(&list)
                        }
                        syntax::Declaration::Precision(_, ty) => self.type_specifier(&ty),
                        syntax::Declaration::Block(block) => self.block(&block),
                        syntax::Declaration::Global(q, _) => self.type_qualifier(&q),
                    },
                    syntax::SimpleStatement::Expression(expr) => {
                        if let Some(expr) = expr {
                            self.expr(&expr);
                        }
                    }
                    syntax::SimpleStatement::Selection(selection) => {
                        self.expr(&selection.cond);
                        match selection.rest {
                            syntax::SelectionRestStatement::Statement(s1) => self.statement(&s1),
                            syntax::SelectionRestStatement::Else(s1, s2) => {
                                self.statement(&s1);
                                self.statement(&s2);
                            }
                        }
                    }
                    syntax::SimpleStatement::Switch(switch) => {
                        self.expr(&switch.head);
                        for s1 in switch.body {
                            self.statement(&s1);
                        }
                    }
                    syntax::SimpleStatement::CaseLabel(case) => match case {
                        syntax::CaseLabel::Case(e1) => self.expr(&e1),
                        syntax::CaseLabel::Def => {}
                    },
                    syntax::SimpleStatement::Iteration(it) => match it {
                        syntax::IterationStatement::While(condition, s1) => {
                            self.condition(&condition);
                            self.statement(&s1);
                        }
                        syntax::IterationStatement::DoWhile(s1, e1) => {
                            self.statement(&s1);
                            self.expr(&e1);
                        }
                        syntax::IterationStatement::For(init, rest, s1) => {
                            match init {
                                syntax::ForInitStatement::Expression(expr) => {
                                    if let Some(expr) = expr {
                                        self.expr(&expr);
                                    }
                                }
                                syntax::ForInitStatement::Declaration(decl) => match *decl {
                                    syntax::Declaration::FunctionPrototype(fp) => {
                                        self.function_prototype(&fp)
                                    }
                                    syntax::Declaration::InitDeclaratorList(list) => {
                                        self.init_declarator_list(&list)
                                    }
                                    syntax::Declaration::Precision(_, ty) => {
                                        self.type_specifier(&ty)
                                    }
                                    syntax::Declaration::Block(block) => self.block(&block),
                                    syntax::Declaration::Global(qualifier, _) => {
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
                    syntax::SimpleStatement::Jump(j) => {
                        if let syntax::JumpStatement::Return(expr) = j {
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

fn all_functions_called_from_main(unit: &syntax::TranslationUnit) -> HashSet<String> {
    struct State {
        functions: HashSet<String>,
    }

    impl State {
        fn new() -> Self {
            Self {
                functions: HashSet::new(),
            }
        }

        fn process_function(&mut self, unit: &syntax::TranslationUnit, name: &str) {
            if self.functions.contains(name) {
                return;
            }
            self.functions.insert(name.to_string());
            if let Some(func) = named_function(unit, name) {
                for called_name in called_functions(func) {
                    self.process_function(unit, called_name.as_str());
                }
            }
        }
    }

    let mut state = State::new();
    state.process_function(unit, "main");
    return state.functions;
}

fn unused_functions(unit: &syntax::TranslationUnit) -> HashSet<String> {
    let declared = HashSet::from_iter(declared_functions(unit));
    let called = all_functions_called_from_main(unit);
    let unused = declared.difference(&called);
    return unused.map(|x| x.to_owned()).collect();
}

fn remove_unused_functions(unit: &syntax::TranslationUnit) -> syntax::TranslationUnit {
    let unused = unused_functions(unit);
    let mut declarations: Vec<syntax::ExternalDeclaration> = Vec::new();
    for declaration in unit {
        match declaration {
            syntax::ExternalDeclaration::FunctionDefinition(func) => {
                if !unused.contains(&func.prototype.name.0) {
                    declarations.push(declaration.clone());
                }
            }
            _ => declarations.push(declaration.clone()),
        }
    }
    return syntax::TranslationUnit::from_non_empty_iter(declarations)
        .expect("Empty declarations (missing main function?)");
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
        let stage = syntax::ShaderStage::parse(glsl);
        assert!(stage.is_ok());
        let unit = stage.unwrap();
        assert_eq!(declared_functions(&unit), ["rotate", "main"]);
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
        let stage = syntax::ShaderStage::parse(glsl);
        assert!(stage.is_ok());
        let unit = stage.unwrap();
        let main = named_function(&unit, "main").expect("missing function main");
        assert_eq!(
            called_functions(main),
            HashSet::from(["func1", "func2"].map(|x| x.to_string()))
        );
        let func1 = named_function(&unit, "func1").expect("missing function func1");
        assert_eq!(called_functions(func1), HashSet::new());

        assert_eq!(
            all_functions_called_from_main(&unit),
            HashSet::from(
                ["cos", "func1", "func2", "main", "mat2", "rotate", "sin", "vec4"]
                    .map(|x| x.to_string())
            )
        );

        assert_eq!(
            unused_functions(&unit),
            HashSet::from(["func3"].map(|x| x.to_string()))
        );

        let modified_unit = remove_unused_functions(&unit);
        assert_eq!(
            declared_functions(&modified_unit),
            ["rotate", "func1", "func2", "main"]
        );
    }
}
