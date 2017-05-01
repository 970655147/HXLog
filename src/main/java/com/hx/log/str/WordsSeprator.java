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
 * �ָ��ַ����Ĺ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 3:01 PM
 */
public final class WordsSeprator implements Iterator<String> {

    /**
     * �������ַ���, �ָ�������һ��λ�õ�ӳ��, ��Ҫ�����ķ��Ŷ�[���������Ŷ�֮�������, ��������]
     */
    private String str;
    private Map<String, Integer> sepToPos;
    private Map<String, String> escapeMap;
    /**
     * ��ǰ������������, ��һ����ȡ���ַ���[����һ����next()���صĽ��, �����õ��ָ���]
     * ����next()����֮��, �����next()���, �Լ���λ��
     * ����next()����֮��, ���������һ��next()�Ľ��, �Լ���λ��
     */
    private int idx;
    private String next;
    private String current;
    private int currentStartIdx;
    private String prev;
    private int prevStartIdx;
    /**
     * �Ƿ��ȡ�ָ���, �Ƿ��ȡ�հ��ַ���
     * ��һ"�غ�"�Ƿ��ǻ�ȡ�ָ����Ļغ�[�����false, ���ʾ��ǰ�غϻ�ȡ�ָ���], ����ĵ���next()֮�����һ���ָ���
     */
    private boolean gotSep;
    private boolean gotEmptyStr;
    private boolean isNextSep;
    private String lastSep;

    // ��ʼ��
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
     * ��ȡ�Ƿ�����һ��Ԫ��, ����������true, �����next()�������ض�Ӧ�Ľ��[�ָ���ַ���, ���߷ָ���], ���򷵻�null
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
     * ��ȡ�ָ�������ַ�������һ�����[�ָ���ַ���, ���߷ָ���]
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
     * ��ȡ��һ��next���÷��صĽ��
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
     * ��ȡ��һ��next���÷��صĽ���Ŀ�ʼλ������
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
     * ��ȡ����һ��next���÷��صĽ��
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
     * ��ȡ����һ��next���÷��صĽ���Ŀ�ʼλ������
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
     * ��ȡ��ǰ������ַ����ĳ���
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
     * ��ȡ��һ��next��Ҫ���صĽ��, ���� ����ƫ�������ȵ�
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
     * ��ȡ��ǰ�ַ�����ʣ���δ�����ַ�������
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
     * ��һ��next()���صĽ�� + ��ȡ��ǰ�ַ�����ʣ���δ�����ַ�������
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
     * ��ȡstr.subString(pos)
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

    // ----------------- �������� -----------------------

    /**
     * fresh���еķָ�����λ��
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
     * fresh�����ķָ�����λ��
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
     * ��str��start����ʼ��ȡ��һ��sep��λ��[��Ҫ����escapeMap��Ӧ����Ŀ]
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
     * ��ȡ��һ���ָ���
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
     * ��ȡ��һ���ָ�����λ��
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
     * ��ȡ�����ķָ�������һ��λ��
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
