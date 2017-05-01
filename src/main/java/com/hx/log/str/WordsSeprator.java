/**
 * file name : WordSeprator.java
 * created at : 3:43:13 PM Mar 22, 2016
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.log.util.Constants;
import com.hx.log.util.InnerTools;
import com.hx.log.util.Tools;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * 分割字符串的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 3:01 PM
 */
public final class WordsSeprator implements Iterator<String> {

    /**
     * 给定的字符串, 分隔符与下一个位置的映射, 需要跳过的符号对[这两个符号对之间的内容, 不做处理]
     */
    private String str;
    private Map<String, Integer> sepToPos;
    private Map<String, String> escapeMap;
    /**
     * 当前遍历到的索引, 下一个待取的字符串[并不一定是next()返回的结果, 可能拿到分隔符]
     * 调用next()方法之后, 缓存的next()结果, 以及其位置
     * 调用next()方法之后, 缓存的上上一个next()的结果, 以及其位置
     */
    private int idx;
    private String next;
    private String current;
    private int currentStartIdx;
    private String prev;
    private int prevStartIdx;
    /**
     * 是否获取分隔符, 是否获取空白字符串
     * 下一"回合"是否是获取分隔符的回合[如果是false, 则表示当前回合获取分隔符], 缓存的调用next()之后的上一个分隔符
     */
    private boolean gotSep;
    private boolean gotEmptyStr;
    private boolean isNextSep;
    private String lastSep;

    // 初始化
    public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap, boolean gotSep, boolean gotEmptyStr) {
        Tools.assert0(str != null, "str can't be null !");

        this.str = str;
        this.escapeMap = escapeMap;
        this.gotSep = gotSep;
        this.gotEmptyStr = gotEmptyStr;
        this.isNextSep = false;
        this.lastSep = null;
        // update at 2016.04.21
        // update 'Map<String, Integer> sepToPos' => 'Set<String> seps', construct 'sepToPos' by this Constructor
        // incase of 'str' startsWith 'sep'
        // keep input order for some confict cond, likes '?' & '??'
        this.sepToPos = new LinkedHashMap<>();
        if (seps != null) {
            for (String sep : seps) {
                sepToPos.put(sep, -1);
            }
        }

        // freshAll, got every 'sep''s right position!
        freshAll();
    }

    public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap, boolean gotSep) {
        this(str, seps, escapeMap, gotSep, false);
    }

    public WordsSeprator(String str, Set<String> seps, Map<String, String> escapeMap) {
        this(str, seps, escapeMap, true, false);
    }

    /**
     * 获取是否还有下一个元素, 如果结果返回true, 则调用next()方法返回对应的结果[分割的字符串, 或者分隔符], 否则返回null
     *
     * @return boolean true if next() will return legal result, or else next() will return null
     * @author Jerry.X.He
     * @date 4/15/2017 2:53 PM
     * @since 1.0
     */
    @Override
    public boolean hasNext() {
        if (next != null) {
            return true;
        }

        // true 				&& 	true
        if ((idx >= str.length()) && (!(isNextSep && (lastSep != null)))) {
            return false;
        }
        if (gotSep) {
            isNextSep = !isNextSep;
            boolean isNowSep = (!isNextSep);
            if (isNowSep) {
                next = lastSep;
                return hasNext();
            }
        }

        String sep = minSep();
        int pos = getPosBySep(sep);

        // incase of '?', '??' [choice which one is decided by 'InputOrder'[see 'minsep'] ], fixed at 2016.09.30
        while ((pos >= 0) && (pos < idx)) {
            fresh(sep);
            sep = minSep();
            pos = getPosBySep(sep);
        }

        String res = null;
        if (pos < 0) {
            res = str.substring(idx);
            idx = str.length();
            lastSep = null;
        } else {
            fresh(sep);
            res = str.substring(idx, pos);
            idx = pos + sep.length();
            lastSep = sep;
        }
        // because 'Constants' denpend on 'WordsSeprator', and 'Tools' denpend on 'Constants'
        // so use 'InnerTools.isEmpty' instead of 'Tools.isEmpty'[cause circle denpency] in case of 'InitException'
        // if 'res.trim' in 'Constants.EMPTY_CONDITIONS', skip it ! [may cause 'some space' loss]
        if ((!gotEmptyStr) && InnerTools.isEmpty0(res)) {
            return hasNext();
        }
        next = res;
        return hasNext();
    }

    /**
     * 获取分割给定的字符串的下一个结果[分割的字符串, 或者分隔符]
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    @Override
    public String next() {
        prev = current;
        if (!hasNext()) {
            return null;
        }

        prevStartIdx = currentStartIdx;
        // lastSep represents there are no more next(), except current next
        currentStartIdx = idx - next.length() - ((lastSep != null) ? lastSep.length() : 0);
        if(gotSep && (!isNextSep)) {
            currentStartIdx += next.length();
        }
        current = next;
        String res = next;
        next = null;
        return res;
    }

    /**
     * 获取上一次next调用返回的结果
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String current() {
        if (!hasNext()) ;
        return current;
    }

    /**
     * 获取上一次next调用返回的结果的开始位置索引
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public int currentStartIdx() {
        if (!hasNext()) ;
        return currentStartIdx;
    }

    /**
     * 获取上上一次next调用返回的结果
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String prev() {
        if (!hasNext()) ;
        return prev;
    }

    /**
     * 获取上上一次next调用返回的结果的开始位置索引
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public int prevStartIdx() {
        if (!hasNext()) ;
        return prevStartIdx;
    }

    /**
     * 获取当前处理的字符串的长度
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public int length() {
        return str.length();
    }

    /**
     * 获取下一个next将要返回的结果, 但是 不会偏移索引等等
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String seek() {
        if (!hasNext()) {
            return null;
        }
        return next;
    }

    /**
     * 获取当前字符串的剩余的未处理字符串部分
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String rest() {
        if (!hasNext()) ;
        return str.substring(currentStartIdx + current.length());
    }

    /**
     * 上一个next()返回的结果 + 获取当前字符串的剩余的未处理字符串部分
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String currentAndRest() {
        if (!hasNext()) ;
        return str.substring(currentStartIdx);
    }

    /**
     * 获取str.subString(pos)
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 2:54 PM
     * @since 1.0
     */
    public String rest(int pos) {
        if ((pos < 0) || (pos >= str.length())) {
            return null;
        }
        return str.substring(pos);
    }

    @Override
    public void remove() {
        throw new RuntimeException("unsupportedOperation !");
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * fresh所有的分隔符的位置
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 2:59 PM
     * @since 1.0
     */
    private void freshAll() {
        for (Entry<String, Integer> entry : sepToPos.entrySet()) {
            fresh(entry.getKey());
        }
    }

    /**
     * fresh给定的分隔符的位置
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 2:59 PM
     * @since 1.0
     */
    private void fresh(String sep) {
        Integer pos = sepToPos.get(sep);
        if (pos != null) {
            sepToPos.put(sep, indexOf(str, sep, pos + 1));
        }
    }

    /**
     * 从str的start处开始获取下一个sep的位置[需要跳过escapeMap对应的条目]
     *
     * @return void
     * @author Jerry.X.He
     * @date 4/15/2017 2:59 PM
     * @since 1.0
     */
    private Integer indexOf(String str, String sep, int start) {
        int idx = start;
        whileLoop:
        while (idx < str.length()) {
            if (escapeMap != null) {
                for (Entry<String, String> entry : escapeMap.entrySet()) {
                    if (str.startsWith(entry.getKey(), idx)) {
                        idx = str.indexOf(entry.getValue(), idx + entry.getKey().length());
                        if (idx < 0) {
                            break whileLoop;
                        }
                        idx += entry.getValue().length();
                        continue whileLoop;
                    }
                }
            }

            if (str.startsWith(sep, idx)) {
                return idx;
            }
            idx++;
        }

        return -1;
    }

    /**
     * 获取下一个分隔符
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 3:00 PM
     * @since 1.0
     */
    private String minSep() {
        int minPos = Integer.MAX_VALUE;
        String minSep = null;
        for (Entry<String, Integer> entry : sepToPos.entrySet()) {
            if ((entry.getValue() >= 0) && (entry.getValue() < minPos)) {
                minPos = entry.getValue();
                minSep = entry.getKey();
            }
        }
        return minSep;
    }

    /**
     * 获取下一个分隔符的位置
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 3:00 PM
     * @since 1.0
     */
    private int minPos() {
        String minSep = minSep();
        return getPosBySep(minSep);
    }

    /**
     * 获取给定的分隔符的下一个位置
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 4/15/2017 3:00 PM
     * @since 1.0
     */
    private int getPosBySep(String sep) {
        return (sep == null) ? -1 : sepToPos.get(sep);
    }

}
