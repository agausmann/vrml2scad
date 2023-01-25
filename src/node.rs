use crate::token::Token;
use chumsky::error::Simple;
use chumsky::primitive::{choice, just};
use chumsky::recursive::{recursive, Recursive};
use chumsky::{select, Parser as _};
use std::rc::Rc;

pub trait Parser<T>: chumsky::Parser<Token, T, Error = Simple<Token>> {}
impl<T, P: chumsky::Parser<Token, T, Error = Simple<Token>>> Parser<T> for P {}

#[derive(Debug, Clone)]
pub enum DefOr<T> {
    Def(String, T),
    Use(String),
    Just(T),
}

impl<T> DefOr<T> {
    pub fn parser(p: impl Parser<T>) -> impl Parser<DefOr<T>> {
        let p = Rc::new(p);

        choice((
            just(Token::Ident("DEF".into()))
                .ignore_then(ident())
                .then(p.clone())
                .map(|(id, t)| Self::Def(id, t)),
            just(Token::Ident("USE".into()))
                .ignore_then(ident())
                .map(Self::Use),
            p.map(Self::Just),
        ))
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

#[derive(Debug, Clone)]
struct Transform {
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
struct Shape {
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

#[derive(Debug, Clone)]
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
