/**
 * file name : StringUtils.java
 * created at : 22:18:22 2016-12-30
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.common.str.WordsSeprator;
import com.hx.common.util.InnerTools;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hx.log.util.Tools.EMPTY_STR;

/**
 * 字符串处理的相关工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:36 PM
 */
public final class StringUtils {

    // disable constructor
    private StringUtils() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * 如果给定的字符串以startsWith开头, 则移除startsWith
     *
     * @param str        给定的字符串
     * @param startsWith 给定的前缀
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:36 PM
     * @since 1.0
     */
    public static String removeIfStartsWith(String str, String startsWith) {
        return InnerTools.removeIfStartsWith(str, startsWith);
    }

    /**
     * 如果给定的字符串以endsWith开头, 则移除endsWith
     *
     * @param str      给定的字符串
     * @param endsWith 给定的后缀
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:54 PM
     * @since 1.0
     */
    public static String removeIfEndsWith(String str, String endsWith) {
        return InnerTools.removeIfEndsWith(str, endsWith);
    }

    /**
     * 如果给定的字符串不以startsWith开头, 则添加startsWith
     *
     * @param str        给定的字符串
     * @param startsWith 给定的前缀
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:55 PM
     * @since 1.0
     */
    public static String addIfNotStartsWith(String str, String startsWith) {
        return InnerTools.addIfNotStartsWith(str, startsWith);
    }

    /**
     * 如果给定的字符串不以endsWith开头, 则添加endsWith
     *
     * @param str      给定的字符串
     * @param endsWith 给定的后缀
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:54 PM
     * @since 1.0
     */
    public static String addIfNotEndsWith(String str, String endsWith) {
        return InnerTools.addIfNotEndsWith(str, endsWith);
    }

    /**
     * 移除掉sb的添加的最后一个分隔符
     *
     * @param sb      给定的字符串
     * @param lastSep 最后一个分隔符
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:55 PM
     * @since 1.0
     */
    public static void removeLastSep(StringBuilder sb, String lastSep) {
        InnerTools.removeLastSep(sb, lastSep);
    }

    /**
     * 判断给定的字符串是否为空
     *
     * @param str 给定的字符串
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 4:56 PM
     * @since 1.0
     */
    public static boolean isEmpty(String str) {
        return (str == null) || Tools.EMPTY_STR_CONDITIONS.contains(str.trim());
    }

    /**
     * 获取str中以start 和end之间的字符串
     *
     * @param str          给定的字符串
     * @param start        给定的start
     * @param end          给定的end
     * @param includeStart 结果是否需要包含start
     * @param includeEnd   结果是否需要包含end
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRange(String str, String start, String end, boolean includeStart, boolean includeEnd) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(start != null, "'start' can't be null ");
        Tools.assert0(end != null, "'end' can't be null ");

        int startIdx = str.indexOf(start);
        if (startIdx == -1) {
            return EMPTY_STR;
        }

        int endIdx = str.indexOf(end, startIdx + start.length());
        if (endIdx == -1) {
            return EMPTY_STR;
        }

        if (!includeStart) {
            startIdx += start.length();
        }
        if (includeEnd) {
            endIdx += end.length();
        }

        return str.substring(startIdx, endIdx);
    }

    /**
     * 获取str中str子串之后的部分
     *
     * @param str     给定的字符串
     * @param start   给定的子串
     * @param include 是否需要包含start
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRangeWithStart(String str, String start, boolean include) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(start != null, "'start' can't be null ");

        int idx = str.indexOf(start);
        if (idx != -1) {
            if (!include) {
                idx += start.length();
            }
            return str.substring(idx);
        }

        return EMPTY_STR;
    }

    /**
     * 获取str中end子串之后的部分
     *
     * @param str     给定的字符串
     * @param end     给定的子串
     * @param include 是否需要包含start
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRangeWithEnd(String str, String end, boolean include) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(end != null, "'end' can't be null ");

        int idx = str.indexOf(end);
        if (idx != -1) {
            if (include) {
                idx += end.length();
            }
            return str.substring(0, idx);
        }

        return EMPTY_STR;
    }

    public static String getStrInRange(String str, String start, String end) {
        return getStrInRange(str, start, end, false, false);
    }

    public static String getStrInRangeInclude(String str, String start, String end) {
        return getStrInRange(str, start, end, true, true);
    }

    public static String getStrInRangeWithStart(String str, String start) {
        return getStrInRangeWithStart(str, start, false);
    }

    public static String getStrInRangeWithStartInclude(String str, String start) {
        return getStrInRangeWithStart(str, start, true);
    }

    public static String getStrInRangeWithEnd(String str, String end) {
        return getStrInRangeWithEnd(str, end, false);
    }

    public static String getStrInRangeWithEndInclude(String str, String end) {
        return getStrInRangeWithEnd(str, end, true);
    }

    // 整合这三类方法, 之前的实现有点冗余			--2015.12.17
//	public static String getStrInRange(String str, String start, String end) {
////		int startIdx = str.indexOf(start);
////		if(startIdx == -1) {
////			return Tools.EMPTY_STR;
////		}
////		
////		int endIdx = str.indexOf(end, startIdx + start.length());
////		if(endIdx == -1) {
////			return Tools.EMPTY_STR;
////		}
////		
////		return str.substring(startIdx + start.length(), endIdx);
//		return getStrInRange0(str, start, end, false, false);
//	}


    /**
     * 空格类字符
     */
    static Set<Character> SPACES = Tools.asSet(Tools.SPACE, Tools.TAB, Tools.CR, Tools.LF);

    /**
     * 将字符串的多个连续的空格转换为一个空格
     * 思路 : 如果str为null  直接返回null
     * 将str中多个相邻的空格替换为一个空格[SPACE]
     * 如果结果的字符串长度为1 并且该字符为空格, 则直接返回空字符串
     * 否则  去掉前后的空格, 返回之间的子字符串
     * 可以直接使用正则进行处理		// str.replaceAll("\\s+", " ");
     *
     * @param str 给定的字符串
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:59 PM
     * @since 1.0
     */
    public static String trimSpacesAsOne(String str) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            if (SPACES.contains(str.charAt(i))) {
                sb.append(Tools.SPACE);
                int nextI = i + 1;
                while ((nextI < str.length()) && SPACES.contains(str.charAt(nextI))) nextI++;
                i = nextI - 1;
                continue;
            }
            sb.append(str.charAt(i));
        }

        if ((sb.length() == 0) || ((sb.length() == 1) && SPACES.contains(sb.charAt(0)))) {
            return EMPTY_STR;
        } else {
            int start = 0, end = sb.length();
            if (SPACES.contains(sb.charAt(start))) {
                start++;
            }
            if (SPACES.contains(sb.charAt(end - 1))) {
                end--;
            }

            return sb.substring(start, end);
        }
    }

    /**
     * 将arr中的字符串 将相邻的多个空格合并为一个
     *
     * @param arr 给定的字符串集合
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String[] trimSpacesAsOne(String[] arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.length; i++) {
            arr[i] = trimSpacesAsOne(arr[i]);
        }

        return arr;
    }

    /**
     * 将arr中的字符串 将相邻的多个空格合并为一个
     *
     * @param arr 给定的字符串集合
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static List<String> trimSpacesAsOne(List<String> arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.size(); i++) {
            arr.set(i, trimSpacesAsOne(arr.get(i)));
        }

        return arr;
    }

    /**
     * 去掉str中的所有空格, 跳过escape相关的字符
     *
     * @param str       给定的字符
     * @param escapeMap 需要跳过的字符pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String trimAllSpaces(String str, Map<Character, Character> escapeMap) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            Character ch = str.charAt(i);
            if ((escapeMap != null) && escapeMap.containsKey(ch)) {
                int prevI = i;
                i = str.indexOf(escapeMap.get(ch), i + 1);
                if (i >= 0) {
                    sb.append(str.substring(prevI, i + 1));
                } else {
                    sb.append(str.substring(prevI));
                    break;
                }
                continue;
            }
            if (SPACES.contains(str.charAt(i))) {
                int nextI = i + 1;
                while ((nextI < str.length()) && SPACES.contains(str.charAt(nextI))) nextI++;
                i = nextI - 1;
                continue;
            }
            sb.append(str.charAt(i));
        }
        return sb.toString();
    }

    public static String trimAllSpaces(String str) {
        return trimAllSpaces(str, null);
    }

    /**
     * 去掉arr中的所有的字符串的所有空格
     *
     * @param arr       给定的字符串集合
     * @param escapeMap 需要跳过的字符pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String[] trimAllSpaces(String[] arr, Map<Character, Character> escapeMap) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.length; i++) {
            arr[i] = trimAllSpaces(arr[i], escapeMap);
        }

        return arr;
    }

    public static String[] trimAllSpaces(String[] arr) {
        return trimAllSpaces(arr, null);
    }

    /**
     * 去掉arr中的所有的字符串的所有空格
     *
     * @param arr       给定的字符串集合
     * @param escapeMap 需要跳过的字符pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static List<String> trimAllSpaces(List<String> arr, Map<Character, Character> escapeMap) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.size(); i++) {
            arr.set(i, trimAllSpaces(arr.get(i), escapeMap));
        }

        return arr;
    }

    public static List<String> trimAllSpaces(List<String> arr) {
        return trimAllSpaces(arr, null);
    }


    /**
     * 去掉str中的给定的字符
     *
     * @param str            给定的字符串
     * @param needBeFiltered 需要被过滤掉的字符
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String filter(String str, Set<Character> needBeFiltered) {
        if (isEmpty(str) || Tools.isEmpty(needBeFiltered)) {
            return null;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (!needBeFiltered.contains(str.charAt(i))) {
                sb.append(str.charAt(i));
            }
        }

        return trimSpacesAsOne(sb.toString());
    }


    /**
     * 向sb中添加str的字符串, 如果需要clean, 首先clean sb中的字符, 如果需要添加crlf, 添加回车
     *
     * @param sb         给定的stringBuilder
     * @param str        需要添加的字符串
     * @param isClean    添加字符串之前是否需要clean sb
     * @param appendCRLF 添加字符串之后 是否需要添加crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 5:04 PM
     * @since 1.0
     */
    public static void append(StringBuilder sb, String str, boolean isClean, boolean appendCRLF) {
        Tools.assert0(sb != null, "'sb' can't be null ");
        if (isClean) {
            sb.setLength(0);
        }
        sb.append(str);
        if (appendCRLF) {
            sb.append(Tools.CRLF);
        }
    }

    public static void append(StringBuilder sb, String str, boolean isClean) {
        append(sb, str, isClean, false);
    }

    public static void append(StringBuilder sb, String str) {
        append(sb, str, false);
    }

    public static void appendCRLF(StringBuilder sb, String str, boolean isClean) {
        append(sb, str, isClean, true);
    }

    public static void appendCRLF(StringBuilder sb, String str) {
        appendCRLF(sb, str, false);
    }


    // add at 2016.05.18
    /**
     * 标准的case
     */
    public static boolean STD_CASE_TO_UPPERCASE = false;

    /**
     * 获取标准的大写 或者小写
     *
     * @param str         给定的字符串
     * @param isUpperCase 是否转换为大写
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:07 PM
     * @since 1.0
     */
    public static String getStdCase(String str, boolean isUpperCase) {
        Tools.assert0(str != null, "'str' can't be null ");
        if (isUpperCase) {
            return str.toUpperCase();
        } else {
            return str.toLowerCase();
        }
    }

    public static String getStdCase(String str) {
        return getStdCase(str, STD_CASE_TO_UPPERCASE);
    }

    //

    /**
     * 判断str01 和str02是否相同[忽略大小写]
     *
     * @param str01 字符串01
     * @param str02 字符串02
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 5:08 PM
     * @since 1.0
     */
    public static boolean equalsIgnoreCase(String str01, String str02) {
        return InnerTools.equalsIgnoreCase(str01, str02);
    }

    /**
     * 如果给定的字符串的首字母是大写的话, 将其转换为小写
     *
     * @param str 给定的字符串
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:09 PM
     * @since 1.0
     */
    public static String lowerCaseFirstChar(String str) {
        Tools.assert0(!isEmpty(str), "'str' is null ");
        if (str.length() == 1) {
            return str.toLowerCase();
        }
        if (Character.isUpperCase(str.charAt(0))) {
            return Character.toLowerCase(str.charAt(0)) + str.substring(1);
        }

        return str;
    }

    public static String upperCaseFirstChar(String str) {
        Tools.assert0(((str != null) || (str.length() == 0)), "'str' is null ");
        if (str.length() == 1) {
            return str.toUpperCase();
        }
        if (Character.isLowerCase(str.charAt(0))) {
            return Character.toUpperCase(str.charAt(0)) + str.substring(1);
        }

        return str;
    }


    // add at 2016.08.11

    /**
     * 替换给定的字符串为目标字符串
     * 为了增加HXAttrHandler.replaceO[replaceOriginal]而添加
     *
     * @param str 给定的字符串
     * @param src 需要替换的原字符串
     * @param dst 替换到的目标字符串
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:11 PM
     * @since 1.0
     */
    public static String replaceO(String str, String src, String dst) {
        Tools.assert0(str != null, "'str' can't be null !");
        Tools.assert0(src != null, "'src' can't be null !");
        Tools.assert0(dst != null, "'dst' can't be null !");

        StringBuilder sb = new StringBuilder(str.length());
        int idx = 0;
        while (idx >= 0) {
            int nextSrc = str.indexOf(src, idx);
            if (nextSrc >= 0) {
                sb.append(str.substring(idx, nextSrc));
                sb.append(dst);
                idx = nextSrc + src.length();
            } else {
                sb.append(str.substring(idx));
                idx = -1;
            }
        }

        return sb.toString();
    }

    /**
     * 同时替换多个字符串
     * [这里 可能会出现WordSeprator的一些问题, 因此 可以借此机会修正修正]
     *
     * @param str    给定的字符串
     * @param mapper 需要映射的字符串kv对
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:12 PM
     * @since 1.0
     */
    public static String replaceO(String str, Map<String, String> mapper) {
        Tools.assert0(str != null, "'str' can't be null !");
        Tools.assert0(mapper != null, "'mapper' can't be null !");

        StringBuilder sb = new StringBuilder(str.length());
        WordsSeprator sep = new WordsSeprator(str, mapper.keySet(), null, true, true);

        boolean isSep = false;
        while (sep.hasNext()) {
            if (!isSep) {
                sb.append(sep.next());
            } else {
                String sepNow = sep.next();
                sb.append(mapper.get(sepNow));
            }
            isSep = !isSep;
        }

        return sb.toString();
    }

    // add at 2016.11.23

    /**
     * 判断给定的line是否是单行注释[//, --, #, ;]
     *
     * @param line 给定的行
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 5:12 PM
     * @since 1.0
     */
    public static boolean isCommentLine(String line) {
        if (isEmpty(line)) {
            return false;
        }

        String trimmed = line.trim();
        int lim = trimmed.length() < Constants.COMMENT_MAX_LEN ? trimmed.length() : Constants.COMMENT_MAX_LEN;
        for (int i = 1; i <= lim; i++) {
            String sub = line.substring(0, i);
            if (Constants.COMMENT_MARKS.contains(sub)) {
                return true;
            }
        }
        return false;
    }


    /**
     * 转义给定的字符串
     *
     * @param str            给定的字符串
     * @param needToBeFormat 需要转义的字符列表
     * @param transferChar   转义字符
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:45 PM
     * @since 1.0
     */
    public static String transfer(String str, Set<Character> needToBeFormat, Character transferChar) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeFormat.contains(str.charAt(i))) {
                sb.append(transferChar);
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }

    public static String transfer(String str) {
        return transfer(str, InnerTools.NEED_BE_TRANSFER, '\\');
    }

    /**
     * 转义给定的字符串
     *
     * @param str            给定的字符串
     * @param needToBeFormat 需要转义的字符映射
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:46 PM
     * @since 1.0
     */
    public static String transfer(String str, Map<Character, Character> needToBeFormat) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeFormat.containsKey(str.charAt(i))) {
                sb.append(needToBeFormat.get(str.charAt(i)));
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }

    /**
     * 去除格式化字符串   为每一个'"', '\' 前面加上一个转义字符['\']
     *
     * @param str              给定的字符串
     * @param needToBeDeformat 需要去转义的字符列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:48 PM
     * @since 1.0
     */
    public static String detransfer(String str, Set<Character> needToBeDeformat) {
        if (isEmpty(str)) {
            return null;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeDeformat.contains(str.charAt(i))) {
                sb.append(str.charAt(++i));
                continue;
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }


}
