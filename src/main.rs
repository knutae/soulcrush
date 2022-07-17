use glsl::parser::Parse;
use glsl::syntax::ExternalDeclaration;
use glsl::syntax::ShaderStage;
use glsl::syntax::TranslationUnit;
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
}
