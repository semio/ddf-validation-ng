import daggy from 'daggy';
import S from 'sanctuary';
import arcsecond from "arcsecond";

const { 
    many1, 
    many,
    choice,
    char,
    sequenceOf,
    digit, 
    letter,
    endOfInput
 } = arcsecond

const Result = daggy.taggedSum('Result', {
    Ok: ['data'],
    Error: ['msg']
})

const alphaNum = choice([
    letter,
    digit,
])

const alphaNumUnderscore = choice([
    alphaNum,
    char('_')
])

const identifier = sequenceOf([
    alphaNum,
    many(alphaNumUnderscore)
]).map((x) => x.flat())

const parser = sequenceOf([
    identifier,
    endOfInput
])

const parse = (x) => parser.fork(
    x, 
    (error,) => {
        return Result.Error(error)
    },
    (result,) => {
        let [res, ] = result
        return Result.Ok(res.join(''))
    }
)

const res = parse('123123123')
const res2 = parse('!apeint')
const res3 = parse('ap_eint')
const res4 = parse('_apeint')

console.log(res)
console.log(res2)
console.log(res3)
console.log(res4)