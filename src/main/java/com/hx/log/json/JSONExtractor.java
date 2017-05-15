/**
 * file name : JSONExtractor.java
 * created at : 上午10:29:32 2016年8月13日
 * created by 970655147
 */

package com.hx.log.json;

import com.hx.common.math.Eval;
import com.hx.common.str.WordsSeprator;
import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.json.interf.JSON;
import com.hx.log.idx.idx_iterator.*;
import com.hx.common.interf.idx.IdxIterator;
import com.hx.log.util.Tools;

import java.util.*;

import static com.hx.log.util.Log.err;

/**
 * 通过给定的pattern, 从给定的JSON中提取数据
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 3:04 PM
 */
public final class JSONExtractor {

    // disable constructor
    private JSONExtractor() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * '?'匹配一个任意字符
     */
    public static final Character MATCH_ONE = '?';
    /**
     * '*'匹配多个任意字符
     */
    public static final Character MATCH_MULTI = '*';
    /**
     * 合法的通配符列表
     */
    public static final char[] WILDCARDS = new char[]{MATCH_ONE, MATCH_MULTI};
    /**
     * 匹配单个字符的通配符的索引
     */
    public static final int MATCH_ONE_IDX = 0;
    /**
     * 匹配多个字符的通配符的索引
     */
    public static final int MATCH_MULTI_IDX = 1;

    /**
     * eval 使用的前缀
     */
    public static final String EVAL_STARTS = "eval#";
    /**
     * 指代当前JSON
     */
    public static final String $THIS = "$this";
    /**
     * 如果context下的JSON是数组, 那么$len表示数组的长度
     */
    public static final String $LEN = "$len";
    /**
     * 连接属性的分隔符
     */
    public static final String $CONCATE = ".";
    /**
     * 数组边界的左分隔符
     */
    public static final String $ARR_LEFT_BRACKET = "[";
    /**
     * 数组边界的右分隔符
     */
    public static final String $ARR_RIGHT_BRACKET = "]";
    /**
     * 数组边界 start, end 的分隔符
     */
    public static final String $ARR_RANGE_SEP = ",";
    /**
     * pattern中分隔符
     */
    public static final Set<String> OPERAND_SEPS = Tools.asSet($CONCATE, $ARR_RANGE_SEP,
            $ARR_LEFT_BRACKET, $ARR_RIGHT_BRACKET);
    /**
     * pattern分词过程中需要跳过的pair
     */
    public static final Map<String, String> OPERAND_ESCAPE_MAP = Tools.asMap(
            new String[]{"'", "\""},
            new String[]{"'", "\""}
    );

    /**
     * 各个分隔符的或操作符
     */
    public static final String PATTERN_SEP = "\\|";

    /**
     * 根据给定的pattern, 提取相关数据
     * 增加对于多个pattern的处理
     *
     * @param json    给定的JSON
     * @param pattern 提取数据的pattern
     * @return com.hx.json.JSONArray
     * @author Jerry.X.He
     * @date 5/5/2017 3:10 PM
     * @since 1.0
     */
    public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
        Tools.assert0(json != null, "json can't be null !");
        Tools.assert0(pattern != null, "pattern can't be null !");

        String[] subPatterns = pattern.split(PATTERN_SEP);
        for (int i = 0; i < subPatterns.length; i++) {
            JSONArray res = extractInfoFromJSON0(json, subPatterns[i]);
            if (!Tools.isEmpty(res)) {
                return res;
            }
        }

        return new JSONArray();
    }

    /**
     * 根据给定的pattern, 以及给定的数组的长度, 获取给定的IdxIterator
     *
     * @param pattern 给定的pattern
     * @param len     给定的元素个数
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 3:14 PM
     * @since 1.0
     */
    public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
        Tools.assert0(pattern != null, "pattern can't be null !");
        pattern = preparePattern(pattern);

        int lenLen = String.valueOf(len).length();
        int needCut = pattern.length() - lenLen;
        // too long, and can't compitable
        if (needCut > 0) {
            for (int i = 0; i < needCut; i++) {
                if (!Tools.contains(WILDCARDS, pattern.charAt(i))) {
                    return NoneIdxIterator.getInstance();
                }
            }
        }

        String trimedPattern = (needCut > 0) ? pattern.substring(needCut) : pattern;
        int idxOfStar = trimedPattern.indexOf(MATCH_MULTI);
        boolean containsStar = idxOfStar >= 0;
        if (!containsStar) {
            return getIdxIteratorByPattern0(pattern, len);
            // incase of contains '*', padding [1-maxPadding] '?', then concate the 'idxIterator' with 'ChainOfIdxIterator'
        } else {
            String before = trimedPattern.substring(0, idxOfStar);
            String after = trimedPattern.substring(idxOfStar + 1);
            int maxPadding = lenLen - (pattern.length() - 1);

            IdxIteratorChain chain = new IdxIteratorChain();
            for (int i = 1; i <= maxPadding; i++) {
                String paddinged = paddingQuestioned(before, after, String.valueOf(MATCH_ONE), i);
                chain.add(getIdxIteratorByPattern0(paddinged, len));
            }
            return chain;
        }
    }

    /**
     * 预处理pattern
     * 1. 防止类似的情况发生 "**", "*?"
     *
     * @param pattern 给定的的pattern
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 3:11 PM
     * @since 1.0
     */
    public static String preparePattern(String pattern) {
        StringBuilder sb = new StringBuilder(pattern.length());
        boolean starAppeared = false;
        for (int i = 0, len = pattern.length(); i < len; i++) {
            char ch = pattern.charAt(i);
            // trimAll
            if (Character.isWhitespace(ch)) {
                continue;
            }
            if (!Tools.contains(WILDCARDS, ch)) {
                sb.append(ch);
                continue;
            }

            // '??*??'
            int nextI = i + 1;
            boolean containsStar = false;
            if (MATCH_MULTI == ch) {
                containsStar = true;
            }
            while ((nextI < len) && (Tools.contains(WILDCARDS, pattern.charAt(nextI)))) {
                if (MATCH_MULTI == pattern.charAt(nextI)) {
                    containsStar = true;
                }
                nextI++;
            }

            if (containsStar) {
                sb.append(MATCH_MULTI);
                if (starAppeared) {
                    throw new RuntimeException("'JSONExtractor' can only exists one '*' in pattern !");
                }
                starAppeared = true;
            } else {
                sb.append(pattern.substring(i, nextI));
            }
            i = nextI - 1;
        }

        return sb.toString();
    }

    // ----------------------- assist method --------------------------------

    /**
     * 根据给定的pattern, 提取相关数据
     * 1. 预处理数据, 解析pattern生成OperationChain
     * 2. 处理核心业务['$this.category.xx', '$this']
     * 3. 返回获取到的数据
     *
     * @param json    给定的JSON
     * @param pattern 提取数据的pattern, 确保pattern预处理之后只会出现一个'*'
     * @return com.hx.json.JSONArray
     * @author Jerry.X.He
     * @date 5/5/2017 3:10 PM
     * @since 1.0
     */
    private static JSONArray extractInfoFromJSON0(JSON json, String pattern) {
        pattern = Tools.trimAllSpaces(pattern);
        Operand head = parseOperand(pattern);

        JSONArray res = new JSONArray();
        Operand prev = head;
        Operand cur = prev.next;
        List<JSON> curJsonList = Tools.asList(json);
        // incase of '$this.category.xx'
        while (cur != null) {
            // cut off
            if (curJsonList.isEmpty()) {
                break;
            }

            // Collection's use may optimize [Queue instead?]
            List<JSON> nextJsonList = new ArrayList<>(curJsonList.size());
            // 			  prev		   cur
            //              |			|
            // incase of '$this[1, 4].category.url'
            // incase of '$this[1, 4].category[2].url'
            // or collect data in 'operandChain of final'
            if (prev.isArray()) {
                for (JSON curJson : curJsonList) {
                    Tools.assert0(curJson.isArray(), "expect an JSONArray : " + curJson.toString());

                    // ...
                    // 不允许arr[idx01][idx02], 因此默认约定prevArr中取出来的数据均为JSONObject
                    // 如果prevArr中获取大的数据为JSONArray 或者其他类型会抛出异常
                    // 如果 prevArrObj不是JSON格式的数据结构, 抛出异常
                    JSONArray prevArr = (JSONArray) curJson;
                    IdxIterator idxIterator = getIdxIteratorByOperand(prev, prevArr);
                    // collect data
                    if (cur.next == null) {
                        collectPrevArr(prevArr, idxIterator, cur, res);
                        // iterate update
                    } else {
                        while (idxIterator.hasNext()) {
                            JSONObject prevArrObj = (JSONObject) prevArr.get(idxIterator.next());
                            if (!Tools.isEmpty(prevArrObj)) {
                                nextJsonList.add((JSON) prevArrObj.opt(cur.key));
                            }
                        }
                    }
                }
                // 			  prev     cur
                //              |		|
                // incase of '$this.category.url'
                // incase of '$this.category[2].url'
                // or collect data in 'operandChain of final'
            } else {
                for (JSON curJson : curJsonList) {
                    Tools.assert0(!curJson.isArray(), "expect an JSONObject : " + curJson.toString());

                    JSONObject prevObj = (JSONObject) curJson;
                    // collect data
                    if (cur.next == null) {
                        collectPrevObj(prevObj, cur, res);
                        // iterate update
                    } else {
                        if (cur.isArray()) {
                            curJson = prevObj.optJSONArray(cur.key);
                        } else {
                            curJson = prevObj.optJSONObject(cur.key);
                        }
                        if (curJson != null) {
                            nextJsonList.add(curJson);
                        }
                    }
                }
            }
            curJsonList = nextJsonList;

            prev = cur;
            cur = cur.next;
        }
        // incase of '$this[xx, xx]', '$this'
        if (prev == head) {
            collect$This(curJsonList, prev, res);
        }

        return res;
    }

    /**
     * 解析pattern 生成Operand链
     *
     * @param pattern 给定的pattern
     * @return com.hx.log.json.JSONExtractor.Operand
     * @author Jerry.X.He
     * @date 5/5/2017 3:25 PM
     * @since 1.0
     */
    private static Operand parseOperand(String pattern) {
        WordsSeprator sep = new WordsSeprator(pattern, OPERAND_SEPS, OPERAND_ESCAPE_MAP, true);
        if (!sep.hasNext()) {
            return null;
        }

        Tools.assert0($THIS.equals(sep.next()), "pattern must starts with '$this' !");
        Operand head = new Operand($THIS, null, null, null);
        parseArrRange(sep, head);
        Operand prev = head;
        while (sep.hasNext() && $CONCATE.equals(sep.next())) {
            String curOpeName = sep.next();
            Operand curOpe = new Operand(curOpeName, null, null, null);
            parseArrRange(sep, curOpe);
            prev.next = curOpe;
            prev = curOpe;
        }
        if (sep.hasNext() && (!$CONCATE.equals(sep.current()))) {
            Tools.assert0("not good format around, expect an '.' : '" + sep.rest() + "'");
            ;
        }

        return head;
    }

    /**
     * 解析给定的ope的数组范围参数[left, right] or [left]
     *
     * @param sep tokenizer
     * @param ope 当前Operand
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 3:25 PM
     * @since 1.0
     */
    private static void parseArrRange(WordsSeprator sep, Operand ope) {
        if ($ARR_LEFT_BRACKET.equals(sep.seek())) {
            sep.next();                                    // '['
            String ope01 = sep.next();
            String commonOrRightBracket = sep.next();    // ',', ']'
            if ($ARR_RANGE_SEP.equals(commonOrRightBracket)) {
                String ope02 = sep.next();
                Tools.assert0(!Tools.isEmpty(ope01), "leftOperand can't be null !");
                Tools.assert0(!Tools.isEmpty(ope02), "rightOperand can't be null !");

                ope.left = ope01;
                ope.right = ope02;
                Tools.assert0($ARR_RIGHT_BRACKET.equals(sep.next()), "expect a ']', around : '" + sep.rest() + "' ");
            } else if ($ARR_RIGHT_BRACKET.equalsIgnoreCase(commonOrRightBracket)) {
                Tools.assert0(!Tools.isEmpty(ope01), "leftOperand can't be null !");
                ope.left = ope01;
            } else {
                Tools.assert0("expect a ',' or ']', around : '" + sep.rest() + "' ");
            }
        }
    }

    /**
     * 根据给定的Operand, 生成IdxIterator
     *
     * @param prev
     * @param prevArr
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 3:26 PM
     * @since 1.0
     */
    private static IdxIterator getIdxIteratorByOperand(Operand prev, JSONArray prevArr) {
        String left = prev.left, right = prev.right;
        IdxIterator res = null;

        // just only one bounds
        if (right == null) {
            left = trimQuote(left);
            if (left.startsWith(EVAL_STARTS)) {
                left = left.replace($LEN, String.valueOf(prevArr.size()));
                int start = Eval.eval(left.substring(EVAL_STARTS.length()));
                res = new SingleIdxIterator(start);
            } else {
                res = getIdxIteratorByPattern(left, prevArr.size());
            }
            // there are two bounds
        } else {
            // trim '' or ""
            left = trimQuote(left);
            right = trimQuote(right);
            if (left.startsWith(EVAL_STARTS)) {
                // inject $len
                left = left.replace($LEN, String.valueOf(prevArr.size()));
                if (right.startsWith(EVAL_STARTS)) {
                    right = right.replace($LEN, String.valueOf(prevArr.size()));
                    int start = Eval.eval(left.substring(EVAL_STARTS.length()));
                    int end = Eval.eval(right.substring(EVAL_STARTS.length()));
                    res = new RangeIdxIterator(start, end);
                } else {
                    Tools.assert0("not compatiable useage 'eval' & 'wildcard' !");
                }
            } else {
                if (right.startsWith(EVAL_STARTS)) {
                    Tools.assert0("not compatiable useage 'eval' & 'wildcard' !");
                } else {
                    int start = formatLeft(left, prevArr);
                    int end = formatRight(right, prevArr);
                    res = new RangeIdxIterator(start, end);
                }
            }
        }

        if (res == null) {
            return NoneIdxIterator.getInstance();
        }
        return legalization(res, prevArr);
    }

    /**
     * 使得给定的索引合法化, 加上一个上限
     *
     * @param idxIterator 给定的idxIterator
     * @param arr         context的数组
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 3:28 PM
     * @since 1.0
     */
    private static IdxIterator legalization(IdxIterator idxIterator, JSONArray arr) {
        return new UpperBoundsIdxIterator(idxIterator, arr.size(), false);
    }

    // 格式化[left, right], left, right[原则为将左边的?变为0, 右边的?变为9, 左边的*去掉, 右边的*填充9]

    /**
     * 获取符合左边界的pattern的最小的数字, 将?转换为0, 去掉*
     *
     * @param left 左边界的pattern
     * @param arr  给定的数组
     * @return int
     * @author Jerry.X.He
     * @date 5/5/2017 3:20 PM
     * @since 1.0
     */
    private static int formatLeft(String left, JSONArray arr) {
        StringBuilder sb = new StringBuilder(left.length());
        for (int i = 0, len = left.length(); i < len; i++) {
            char ch = left.charAt(i);
            if (ch == MATCH_MULTI) {
                continue;
            }
            if (ch == MATCH_ONE) {
                sb.append("0");
            } else {
                sb.append(ch);
            }
        }
        // compatiable with '*'
        if (sb.length() == 0) {
            sb.append("0");
        }

        return Integer.parseInt(sb.toString());
    }

    /**
     * 获取符合右边界的pattern的最大的数字, 将?转换为9, *转换为填充9[填充到arr.length的长度]
     *
     * @param right 右边界的pattern
     * @param arr   给定的数组
     * @return int
     * @author Jerry.X.He
     * @date 5/5/2017 3:20 PM
     * @since 1.0
     */
    private static int formatRight(String right, JSONArray arr) {
        StringBuilder sb = new StringBuilder(right.length());
        int lenLen = String.valueOf(arr.size()).length();
        int maxPadding = lenLen - (right.length() - 1);

        for (int i = 0, len = right.length(); i < len; i++) {
            char ch = right.charAt(i);
            if (ch == MATCH_MULTI) {
                for (int paddingCnt = 0; paddingCnt < maxPadding; i++) {
                    sb.append("9");
                }
                continue;
            }
            if (ch == MATCH_ONE) {
                sb.append("9");
            } else {
                sb.append(ch);
            }
        }

        return Integer.parseInt(sb.toString());
    }

    /**
     * 如果str以引号开头结尾, 则去掉引号
     *
     * @param str 给定的字符串
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 3:20 PM
     * @since 1.0
     */
    private static String trimQuote(String str) {
        if ((str.startsWith("'") && str.endsWith("'"))
                || (str.startsWith("\"") && str.endsWith("\""))) {
            str = str.substring(1, str.length() - 1);
        }
        return str;
    }

    /**
     * 没有*的场景, 获取给定的pattern匹配的所有的Idx匹配的IdxIterator
     * 不可能匹配到数据的场景, 以及精确匹配的场景
     * 根据对应的通配符 以及pattern生成SomeBitIncIdxIterator
     *
     * @param pattern 给定的pattern
     * @param len     目标数组的长度
     * @return com.hx.common.interf.idx.IdxIterator
     * @author Jerry.X.He
     * @date 5/5/2017 3:18 PM
     * @since 1.0
     */
    private static IdxIterator getIdxIteratorByPattern0(String pattern, int len) {
        int lenLen = String.valueOf(len).length();
        int needCut = pattern.length() - lenLen;
        // too long, and can't compitable
        if (needCut > 0) {
            for (int i = 0; i < needCut; i++) {
                if (pattern.charAt(i) != MATCH_ONE) {
                    return NoneIdxIterator.getInstance();
                }
            }
        }

        String trimedPattern = (needCut > 0) ? pattern.substring(needCut) : pattern;
        StringBuilder startBuilder = new StringBuilder(trimedPattern.length());
        StringBuilder endBuilder = new StringBuilder(trimedPattern.length());
        BitSet bs = new BitSet();
        boolean isSingle = true;
        // ??2		// higher -> lower
        for (int i = 0, _len = trimedPattern.length(); i < _len; i++) {
            char ch = trimedPattern.charAt(i);
            if (ch == MATCH_ONE) {
                startBuilder.append("0");
                endBuilder.append("9");
                bs.set(_len - i - 1);
                isSingle = false;
            } else {
                startBuilder.append(ch);
                endBuilder.append(ch);
            }
        }

        // accurate match
        int start = Integer.parseInt(startBuilder.toString());
        if (isSingle) {
            return new SingleIdxIterator(start);
        }

        // nonAccurate match
        int end = Integer.parseInt(endBuilder.toString());
        IdxIteratorChain chain = new IdxIteratorChain().add(new SomeBitIncIdxIterator(start, end, bs)).add(new SingleIdxIterator(end));
        return new UpperBoundsIdxIterator(chain, len);
    }

    /**
     * 在before, after之间填充times个padding
     *
     * @param before  prefix
     * @param after   suffix
     * @param padding 需要padding的字符串
     * @param times   需要padding的字符串的次数
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 3:29 PM
     * @since 1.0
     */
    private static String paddingQuestioned(String before, String after, String padding, int times) {
        StringBuilder sb = new StringBuilder(before.length() + after.length() + padding.length() * times);
        sb.append(before);
        for (int i = 0; i < times; i++) {
            sb.append(padding);
        }
        sb.append(after);
        return sb.toString();
    }

    /**
     * 收集父级操作数为数组 当前操作数为数组 或者对象的情况
     *
     * @param prevArr     父级JSOANrray
     * @param idxIterator 父级Operand解析出来的idxIterator[getIdxIteratorByOperand(prev, prevArr)]
     * @param cur         当前Operand
     * @param res         目标结果集合
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 3:30 PM
     * @since 1.0
     */
    private static void collectPrevArr(JSONArray prevArr, IdxIterator idxIterator, Operand cur, JSONArray res) {
        // $this[1, 2].category[1, 3]
        if (cur.isArray()) {
            // ...
            while (idxIterator.hasNext()) {
//				JSONArray arr = (JSONArray) prevArr.get(idxIterator.next());
                JSONObject theNObj = (JSONObject) prevArr.get(idxIterator.next());
                JSONArray arr = theNObj.getJSONArray(cur.key);
                if (!Tools.isEmpty(arr)) {
                    collectEndsWithArray(arr, cur, res);
                } else {
                    err("err while got JSONArray from : " + prevArr);
                }
            }
            // $this[1,2].url
        } else {
            while (idxIterator.hasNext()) {
                JSONObject obj = (JSONObject) prevArr.get(idxIterator.next());
                if (!Tools.isEmpty(obj)) {
                    Object resEle = obj.opt(cur.key);
                    if (resEle != null) {
                        res.add(resEle);
                    }
                } else {
                    err("err while got JSONObject from : " + prevArr);
                }
            }
        }
    }

    /**
     * 收集父级操作数为JSONObject, 当前操作数为数组 或者对象的情况
     *
     * @param prevObj 父级JSONObject
     * @param cur     当前Operand
     * @param res     目标收集的集合
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 3:42 PM
     * @since 1.0
     */
    private static void collectPrevObj(JSONObject prevObj, Operand cur, JSONArray res) {
        // $this.category[2, 4]
        if (cur.isArray()) {
            // ...
            JSONArray arr = (JSONArray) prevObj.get(cur.key);
            if (!Tools.isEmpty(arr)) {
                collectEndsWithArray(arr, cur, res);
            } else {
                err("err while got JSONArray from : " + prevObj);
            }
            // $this.url
        } else {
            Object resEle = prevObj.opt(cur.key);
            if (resEle != null) {
                res.add(resEle);
            }
        }
    }

    /**
     * 将arr中cur对应的所有的元素 收集到res中
     *
     * @param arr 收集arr中的对象
     * @param cur 当前operand
     * @param res 目标收集的集合
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 3:48 PM
     * @since 1.0
     */
    private static void collectEndsWithArray(JSONArray arr, Operand cur, JSONArray res) {
        IdxIterator curIdxIterator = getIdxIteratorByOperand(cur, arr);
        while (curIdxIterator.hasNext()) {
            Object resEle = arr.opt(curIdxIterator.next());
            if (resEle != null) {
                res.add(resEle);
            }
        }
    }

    /**
     * 处理 '$this[xx, xx]', '$this' 的场景
     *
     * @param curJsonList 当前待处理的JSON列表
     * @param prev        当前待处理的前一个Operand
     * @param res         收集的目标集合
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 3:49 PM
     * @since 1.0
     */
    private static void collect$This(List<JSON> curJsonList, Operand prev, JSONArray res) {
        if (!Tools.isEmpty(curJsonList)) {
            if (prev.isArray()) {
                for (JSON curJson : curJsonList) {
                    Tools.assert0(curJson.isArray(), "expect an JSONArray : " + curJson.toString());
                    JSONArray prevArr = (JSONArray) curJson;
                    IdxIterator idxIterator = getIdxIteratorByOperand(prev, prevArr);
                    // collect data
                    while (idxIterator.hasNext()) {
                        Object resEle = prevArr.opt(idxIterator.next());
                        if (resEle != null) {
                            res.add(resEle);
                        }
                    }
                }
            } else {
                for (JSON curJson : curJsonList) {
                    Tools.assert0(!curJson.isArray(), "expect an JSONObject : " + curJson.toString());
                    Object resEle = curJson;
                    if (resEle != null) {
                        res.add(resEle);
                    }
                }
            }
        }
    }

    // --------------- bean --------------------

    /**
     * 解析pattern获取到的Operand
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/5/2017 3:50 PM
     */
    private static class Operand {
        /**
         * 指定的JSONObject, JSONArray的key
         */
        String key;
        /**
         * 下限pattern
         */
        String left;
        /**
         * 上限pattern
         */
        String right;
        /**
         * 下一个Operand
         */
        Operand next;

        /**
         * 初始化
         *
         * @param key   指定的JSONObject, JSONArray的key
         * @param left  下限pattern
         * @param right 上限pattern
         * @param next  下一个Operand
         * @since 1.0
         */
        public Operand(String key, String left, String right, Operand next) {
            this.key = key;
            this.left = left;
            this.right = right;
            this.next = next;
        }

        /**
         * 当前Operand操作数是否是数组
         *
         * @return boolean
         * @author Jerry.X.He
         * @date 5/5/2017 3:52 PM
         * @since 1.0
         */
        public boolean isArray() {
            return left != null;
        }

        /**
         * for debug ..
         *
         * @return java.lang.String
         * @author Jerry.X.He
         * @date 5/5/2017 3:52 PM
         * @since 1.0
         */
        public String toString() {
            return new JSONObject()
                    .element("key", key).element("left", left).element("right", right)
                    .toString();
        }
    }

    /**
     * 运行时得到的Operand [not use]
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 5/5/2017 3:53 PM
     */
    private static class RuntimeOperand {
        /**
         * 指定的JSONObject, JSONArray的key
         */
        String key;
        /**
         * left, right以及上下文的信息生成的idxIterator
         */
        IdxIterator idxIterator;
        /**
         * 下一个Operand
         */
        RuntimeOperand next;

        /**
         * 初始化
         *
         * @param key         指定的JSONObject, JSONArray的key
         * @param idxIterator left, right以及上下文的信息生成的idxIterator
         * @param next        下一个Operand
         * @since 1.0
         */
        public RuntimeOperand(String key, IdxIterator idxIterator, RuntimeOperand next) {
            this.key = key;
            this.idxIterator = idxIterator;
            this.next = next;
        }

        /**
         * 当前Operand操作数是否是数组
         *
         * @return boolean
         * @author Jerry.X.He
         * @date 5/5/2017 3:52 PM
         * @since 1.0
         */
        public boolean isArray() {
            return idxIterator != null;
        }

        /**
         * for debug ..
         *
         * @return java.lang.String
         * @author Jerry.X.He
         * @date 5/5/2017 3:54 PM
         * @since 1.0
         */
        public String toString() {
            return new JSONObject()
                    .element("key", key).element("isArray", isArray())
                    .toString();
        }
    }

}
