use crate::token::Token;
use chumsky::error::Simple;
use chumsky::primitive::{choice, just};
use chumsky::recursive::{recursive, Recursive};
use chumsky::{select, Parser as _};
use std::io::{self, Write};
use std::rc::Rc;

pub trait Parser<T>: chumsky::Parser<Token, T, Error = Simple<Token>> {}
impl<T, P: chumsky::Parser<Token, T, Error = Simple<Token>>> Parser<T> for P {}

pub trait WriteDef {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()>;
}

#[derive(Debug, Clone)]
pub enum DefOr<T> {
    Def(String, T),
    Use(String),
}

impl<T> DefOr<T> {
    pub fn parser(p: impl Parser<T>) -> impl Parser<DefOr<T>> {
        let p = Rc::new(p);

        choice((
            keyword("DEF")
                .ignore_then(ident())
                .then(p.clone())
                .map(|(id, t)| Self::Def(id, t)),
            keyword("USE").ignore_then(ident()).map(Self::Use),
        ))
    }

    pub fn id(&self) -> &str {
        match self {
            Self::Def(id, _) | Self::Use(id) => id,
        }
    }

    pub fn write_def(&self, w: &mut impl Write) -> io::Result<()>
    where
        T: WriteDef,
    {
        match self {
            Self::Def(id, t) => t.write_def(id, w)?,
            Self::Use(_) => {}
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Transform(Transform),
    Shape(Shape),
}

impl Node {
    pub fn parser() -> impl Parser<Self> {
        recursive(|node| {
            choice((
                Transform::parser(node).map(Self::Transform),
                Shape::parser().map(Self::Shape),
            ))
        })
    }
}

impl WriteDef for Node {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        match self {
            Self::Transform(x) => x.write_def(id, w),
            Self::Shape(x) => x.write_def(id, w),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Transform {
    center: [f32; 3],
    rotation: [f32; 4],
    scale: [f32; 3],
    scale_orientation: [f32; 4],
    translation: [f32; 3],
    children: Vec<DefOr<Node>>,
}

impl Transform {
    fn parser<'a>(node: Recursive<'a, Token, Node, Simple<Token>>) -> impl Parser<Self> + 'a {
        keyword("Transform")
            .to(Transform::default())
            .then(
                TransformField::parser(node)
                    .repeated()
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .foldl(|t, field| match field {
                TransformField::Center(center) => Self { center, ..t },
                TransformField::Rotation(rotation) => Self { rotation, ..t },
                TransformField::Scale(scale) => Self { scale, ..t },
                TransformField::ScaleOrientation(scale_orientation) => Self {
                    scale_orientation,
                    ..t
                },
                TransformField::Translation(translation) => Self { translation, ..t },
                TransformField::Children(children) => Self { children, ..t },
            })
    }
}

impl WriteDef for Transform {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        for child in &self.children {
            child.write_def(w)?;
        }
        // TODO apply center, scale_orientation
        assert_eq!(self.scale_orientation[3], 0.0);
        assert_eq!(self.center, [0.0; 3]);

        write!(w, "module {}() {{ ", id)?;
        if self.translation != [0.0; 3] {
            write!(
                w,
                "translate([{}, {}, {}]) ",
                self.translation[0], self.translation[1], self.translation[2]
            )?;
        }
        if self.rotation[3] != 0.0 {
            write!(
                w,
                "rotate({}, [{}, {}, {}]) ",
                self.rotation[3], self.rotation[0], self.rotation[1], self.rotation[2]
            )?;
        }
        if self.scale != [1.0; 3] {
            write!(
                w,
                "scale([{}, {}, {}]) ",
                self.scale[0], self.scale[1], self.scale[2]
            )?;
        }

        write!(w, "{{ ")?;
        for child in &self.children {
            write!(w, "{}();", child.id())?;
        }
        writeln!(w, " }} }}")?;
        Ok(())
    }
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            center: [0.0; 3],
            rotation: [0.0, 0.0, 1.0, 0.0],
            scale: [1.0; 3],
            scale_orientation: [0.0, 0.0, 1.0, 0.0],
            translation: [0.0; 3],
            children: Vec::new(),
        }
    }
}

enum TransformField {
    Center([f32; 3]),
    Rotation([f32; 4]),
    Scale([f32; 3]),
    ScaleOrientation([f32; 4]),
    Translation([f32; 3]),
    Children(Vec<DefOr<Node>>),
}

impl TransformField {
    fn parser<'a>(node: Recursive<'a, Token, Node, Simple<Token>>) -> impl Parser<Self> + 'a {
        choice((
            keyword("center").ignore_then(vec3()).map(Self::Center),
            keyword("rotation").ignore_then(vec4()).map(Self::Rotation),
            keyword("scale").ignore_then(vec3()).map(Self::Scale),
            keyword("scaleOrientation")
                .ignore_then(vec4())
                .map(Self::ScaleOrientation),
            keyword("translation")
                .ignore_then(vec3())
                .map(Self::Translation),
            keyword("children")
                .ignore_then(
                    DefOr::parser(node)
                        .repeated()
                        .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket)),
                )
                .map(Self::Children),
        ))
    }
}

#[derive(Debug, Clone)]
pub struct Shape {
    appearance: DefOr<Appearance>,
    geometry: DefOr<IndexedFaceSet>,
}

impl Shape {
    fn parser() -> impl Parser<Self> {
        keyword("Shape")
            .ignore_then(
                (keyword("appearance").ignore_then(DefOr::parser(Appearance::parser())))
                    .then(keyword("geometry").ignore_then(DefOr::parser(IndexedFaceSet::parser())))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|(appearance, geometry)| Self {
                appearance,
                geometry,
            })
    }
}

impl WriteDef for Shape {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        self.appearance.write_def(w)?;
        self.geometry.write_def(w)?;
        writeln!(
            w,
            "module {}() {{ color({}) {}(); }}",
            id,
            self.appearance.id(),
            self.geometry.id()
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Appearance {
    material: Material,
}

impl Appearance {
    fn parser() -> impl Parser<Self> {
        keyword("Appearance")
            .ignore_then(
                (keyword("material").ignore_then(Material::parser()))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|material| Self { material })
    }
}

impl WriteDef for Appearance {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        writeln!(
            w,
            "{} = [{},{},{},{}];",
            id,
            self.material.diffuse_color[0],
            self.material.diffuse_color[1],
            self.material.diffuse_color[2],
            1.0 - self.material.transparency
        )
    }
}

#[derive(Debug, Clone)]
struct Material {
    diffuse_color: [f32; 3],
    transparency: f32,
}

impl Material {
    fn parser() -> impl Parser<Self> {
        keyword("Material")
            .ignore_then(
                (keyword("ambientIntensity").then(float()))
                    .ignore_then(keyword("diffuseColor").ignore_then(vec3()))
                    .then_ignore(keyword("emissiveColor").then(vec3()))
                    .then_ignore(keyword("shininess").then(float()))
                    .then_ignore(keyword("specularColor").then(vec3()))
                    .then(keyword("transparency").ignore_then(float()))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|(diffuse_color, transparency)| Self {
                diffuse_color,
                transparency,
            })
    }
}

#[derive(Debug, Clone)]
struct IndexedFaceSet {
    coord: DefOr<Coordinate>,
    coord_index: Vec<i32>,
}

impl IndexedFaceSet {
    fn parser() -> impl Parser<Self> {
        keyword("IndexedFaceSet")
            .ignore_then(
                (keyword("coord").ignore_then(DefOr::parser(Coordinate::parser())))
                    .then(keyword("coordIndex").ignore_then(list(int())))
                    .then_ignore(keyword("normalPerVertex").then(ident()))
                    .then_ignore(keyword("normal").then(DefOr::parser(Normal::parser())))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|(coord, coord_index)| Self { coord, coord_index })
    }
}

impl WriteDef for IndexedFaceSet {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        self.coord.write_def(w)?;

        writeln!(w, "module {}() {{ polyhedron({},[", id, self.coord.id())?;
        for face in self.coord_index.chunks(4) {
            // TODO support quads
            assert_eq!(face[3], -1);
            writeln!(w, "\t[{},{},{}],", face[0], face[1], face[2])?;
        }
        writeln!(w, "]); }}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Coordinate {
    point: Vec<[f32; 3]>,
}

impl Coordinate {
    fn parser() -> impl Parser<Self> {
        keyword("Coordinate")
            .ignore_then(
                keyword("point")
                    .ignore_then(list(vec3()))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|point| Self { point })
    }
}

impl WriteDef for Coordinate {
    fn write_def(&self, id: &str, w: &mut impl Write) -> io::Result<()> {
        writeln!(w, "{} = [", id)?;
        for p in &self.point {
            writeln!(w, "\t[{},{},{}],", p[0], p[1], p[2])?;
        }
        writeln!(w, "];")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Normal {
    vector: Vec<[f32; 3]>,
}

impl Normal {
    fn parser() -> impl Parser<Self> {
        keyword("Normal")
            .ignore_then(
                keyword("vector")
                    .ignore_then(list(vec3()))
                    .delimited_by(just(Token::OpenBrace), just(Token::CloseBrace)),
            )
            .map(|vector| Self { vector })
    }
}

fn vec3() -> impl Parser<[f32; 3]> {
    float().repeated().exactly(3).map(|v| v.try_into().unwrap())
}

fn vec4() -> impl Parser<[f32; 4]> {
    float().repeated().exactly(4).map(|v| v.try_into().unwrap())
}

fn ident() -> impl Parser<String> {
    select! {
        Token::Ident(x) => x,
    }
}

fn keyword(s: &str) -> impl Parser<()> {
    just(Token::Ident(s.into())).ignored()
}

fn int() -> impl Parser<i32> {
    select! {
        Token::Number(x) => x.parse().unwrap(),
    }
}

fn float() -> impl Parser<f32> {
    select! {
        Token::Number(x) => x.parse().unwrap(),
    }
}

fn list<T>(p: impl Parser<T>) -> impl Parser<Vec<T>> {
    let p = Rc::new(p);

    p.clone()
        .chain(just(Token::Comma).ignore_then(p).repeated())
        .then_ignore(just(Token::Comma).or_not())
        .delimited_by(just(Token::OpenBracket), just(Token::CloseBracket))
}
