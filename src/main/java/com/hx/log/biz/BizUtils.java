/**
 * file name : BizUtils.java
 * created at : 22:51:02 2016-12-30
 * created by 970655147
 */

package com.hx.log.biz;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.biz.interf.GetLengthStrHandler;
import com.hx.log.util.Tools;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.hx.log.log.LogPatternUtils.formatLogInfoWithIdx;
import static com.hx.log.util.Log.err;
import static com.hx.log.util.Tools.assert0;

/**
 * 处理日常业务的工具类
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:29 PM
 */
public final class BizUtils {

    // disable constructor
    private BizUtils() {
        assert0("can't instantiate !");
    }


    /**
     * 所有的数字的Character
     */
    public static final Set<Character> NUMBERS = new HashSet<>();

    static {
        for (char i = '0'; i <= '9'; i++) {
            NUMBERS.add(i);
        }
    }

    /**
     * 处理价格, 也可以用于处理提取字符串中的BigDecimal的情况
     * 提取给定的str中的数字, ., 然后转换为 BigDecimal
     *
     * @param str 给定的字符串
     * @return java.math.BigDecimal
     * @author Jerry.X.He
     * @date 5/4/2017 10:30 PM
     * @since 1.0
     */
    public static BigDecimal dealPrice(String str) {
        if (Tools.isEmpty(str)) {
            return Tools.BIGDEC_ZERO;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (NUMBERS.contains(ch) || (ch == '.')) {
                sb.append(ch);
            }
        }
        if (sb.length() == 0) {
            return Tools.BIGDEC_ZERO;
        } else {
            return new BigDecimal(sb.toString());
        }
    }

    /**
     * 处理页数, 也可以用于处理提取字符串中的整数的情况
     * 提取给定的str中的数字, 然后转换为 Integer
     *
     * @param str 给定的字符串
     * @return java.lang.Integer
     * @author Jerry.X.He
     * @date 5/4/2017 10:30 PM
     * @since 1.0
     */
    public static Integer dealPageNum(String str) {
        if (Tools.isEmpty(str)) {
            return Tools.INTE_ZERO;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (NUMBERS.contains(ch)) {
                sb.append(ch);
            }
        }
        if (sb.length() == 0) {
            return Tools.INTE_ZERO;
        } else {
            return new Integer(sb.toString());
        }
    }


    /**
     * 匹配siteUrl 的 regex
     */
    public static final Pattern SITE_URL_PATTERN = Pattern.compile("^(\\w{3,5}://\\w+(\\.\\w+)+?/)(.*)");

    /**
     * 获取站点的首页url
     * http://www.baidu.com/tieba/java/page01.jsp  =>  http://www.baidu.com/
     *
     * @param url 给定的站点的随意url
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 10:31 PM
     * @since 1.0
     */
    public static String getSiteUrl(String url) {
        assert0(!Tools.isEmpty(url), "'url' can't be null ");
        Matcher matcher = SITE_URL_PATTERN.matcher(url);
        if (matcher.matches()) {
            return matcher.group(1);
        }
        return null;
    }

    /**
     * 将绝对/, 相对的url转换为绝对的url
     * 转换 /path & ./path
     *
     * @param siteUrl      给定的站点的url
     * @param relativePath 相对路径
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 10:32 PM
     * @since 1.0
     */
    public static String transformUrl(String siteUrl, String relativePath) {
        assert0(!Tools.isEmpty(siteUrl), "'siteUrl' can't be null ");
        assert0(!Tools.isEmpty(relativePath), "'relativePath' can't be null ");

        if (relativePath.startsWith("/")) {
            return getSiteUrl(siteUrl) + Tools.removeIfStartsWith(relativePath, "/");
        } else if (relativePath.startsWith("./")) {
            return siteUrl.substring(0, siteUrl.lastIndexOf("/") + 1) + Tools.removeIfStartsWith(relativePath, "./");
        } else {
            return relativePath;
        }
    }

    /**
     * 为 nextStageParams 添加 category 的相关参数
     *
     * @param category        当前Category
     * @param nextStageParams 下一个State的参数
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:33 PM
     * @since 1.0
     */
    public static void addNameUrlSite(JSONObject category, JSONObject nextStageParams) {
        assert0(!Tools.isEmpty(category), "'category' can't be null ");
        nextStageParams.put(Tools.NAME, category.getString(Tools.NAME));
        nextStageParams.put(Tools.URL, category.getString(Tools.URL));
        nextStageParams.put(Tools.SITE, nextStageParams.getString(Tools.SITE) + "." + category.getString(Tools.NAME));
    }

    /**
     * 通过产品的数目, 以及每一页显示的产品的数目, 计算页数
     *
     * @param productNum 产品的数量
     * @param numPerPage 每一页的数量
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 10:33 PM
     * @since 1.0
     */
    public static int calcPageNums(int productNum, int numPerPage) {
        return ((productNum - 1) / numPerPage) + 1;
    }

    /**
     * 获取键值对类型的数据对, 添加到headers中
     *
     * @param configFile 目标配置文件
     * @param headers    解析配置文件之后, 将请求头添加到headers
     * @param sep        目标配置文件的分隔符
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:50 PM
     * @since 1.0
     */
    public static void addHeaders(File configFile, Map<String, String> headers, String sep) {
        assert0(configFile != null, "'configFile' can't be null ");
        assert0(headers != null, "'headers' can't be null ");
        assert0(sep != null, "'sep' can't be null ");

        List<String> lines = null;
        try {
            lines = Tools.getContentWithList(configFile);
        } catch (IOException e) {
            // ignore
            err(formatLogInfoWithIdx("did not find the specified configFile : {} !", configFile));
            return;
        }

        for (String line : lines) {
            if (!Tools.isCommentLine(line)) {
                int idx = line.indexOf(sep);
                if (idx > 0) {
                    headers.put(line.substring(0, idx), line.substring(idx + 1));
                }
            }
        }
    }

    /**
     * 解码含有unicode字符串的字符串
     * 遍历一次字符串, 寻找出匹配"\\uxxxx"的字符串, 然后将其解码为字符[unicode -> char]
     * 对于其他的字符不作处理
     *
     * @param str
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:01 PM
     * @since 1.0
     */
    public static String unicodeDecode(String str) {
        assert0(str != null, "'str' can't be null ");

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (ch == Tools.SLASH) {
                if (str.charAt(i + 1) == 'u') {
                    boolean isUnicode = true;
                    for (int j = 0; j < 4; j++) {
                        // '+2' escape '\\u'
                        if (!isHexChar(str.charAt(i + j + 2))) {
                            isUnicode = false;
                            break;
                        }
                    }

                    // 如果"\\u"之后的四个字符可以表示为十六进制的数字, 则将其解码, 并更新i, continue
                    if (isUnicode) {
                        char decoded = Character.valueOf((char) Integer.valueOf(str.substring(i + 2, i + 6), 16).intValue());
                        sb.append(decoded);
                        i += 5;
                        continue;
                    }
                }
            }

            sb.append(ch);
        }

        return sb.toString();
    }

    /**
     * 判断给定的字符是否可表示十六进制[0-9, a-f, A-F]
     *
     * @param ch 给定的字符
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 11:02 PM
     * @since 1.0
     */
    public static boolean isHexChar(char ch) {
        return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
    }


    // ------------ 进制转换相关 --------------------

    /**
     * 默认的获取长度字符串的Handler
     */
    public static final GetLengthStrHandler DEFAULT_GET_LENGTH_STR_HANDLER = new GetLengthStrHandler() {
        public String getLengthStr(long length, String dimen) {
            return length + " " + dimen;
        }
    };

    /**
     * 根据长度, 获取长度的字符串表示
     *
     * @param length 需要计算的单位的量
     * @param dimen  给定的单位
     * @return the string representation
     * @author Jerry.X.He
     * @date 5/4/2017 11:05 PM
     * @since 1.0
     */
    public static String getLengthString(long length, String dimen) {
        return getLengthString(length, dimen, DEFAULT_GET_LENGTH_STR_HANDLER);
    }

    /**
     * 根据需要计算的单位的量 和单位, 以及获取长度字符串的handler, 获取结果
     *
     * @param length              需要计算的单位的量
     * @param dimen               给定的单位
     * @param getLengthStrHandler 获取长度字符串的handler
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:06 PM
     * @since 1.0
     */
    public static String getLengthString(long length, String dimen, GetLengthStrHandler getLengthStrHandler) {
        long transfered = -1;
        if (Tools.equalsIgnoreCase(Tools.BYTE, dimen)) {
            transfered = length;
        } else if (Tools.equalsIgnoreCase(Tools.KB, dimen)) {
            transfered = Tools.getKBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.MB, dimen)) {
            transfered = Tools.getMBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.GB, dimen)) {
            transfered = Tools.getGBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.TB, dimen)) {
            transfered = Tools.getTBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.PB, dimen)) {
            transfered = Tools.getPBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.EB, dimen)) {
            transfered = Tools.getEBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.ZB, dimen)) {
            transfered = Tools.getZBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.YB, dimen)) {
            transfered = Tools.getYBytesByBytes(length);
        } else {
            assert0("unSupported Unit : " + dimen + " !");
        }

        return getLengthStrHandler.getLengthStr(transfered, dimen);
    }

    // 根据字节数, 获取千字节数, 兆字节数, 吉字节数, 踢字节数
    public static long getKBytesByBytes(long bytes) {
        return bytes >> 10;
    }

    public static long getMBytesByBytes(long bytes) {
        return bytes >> 20;
    }

    public static long getGBytesByBytes(long bytes) {
        return bytes >> 30;
    }

    public static long getTBytesByBytes(long bytes) {
        return bytes >> 40;
    }

    public static long getPBytesByBytes(long bytes) {
        return bytes >> 50;
    }

    public static long getEBytesByBytes(long bytes) {
        return bytes >> 60;
    }

    public static long getZBytesByBytes(long bytes) {
        return bytes >> 70;
    }

    public static long getYBytesByBytes(long bytes) {
        return bytes >> 80;
    }


    // add at 2016.05.17
    /**
     * 查询字符串默认的kv分隔符
     */
    public static String PARAM_KV_SEP = "=";
    /**
     * 查询字符串的多个kv对之间的分隔符
     */
    public static String PARAM_PARAM_SEP = "&";

    /**
     * 增加封装get请求的查询字符串
     *
     * @param params 给定的参数列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:07 PM
     * @since 1.0
     */
    public static String encapQueryString(Map<String, String> params) {
        return encapQueryString0(params, PARAM_KV_SEP, PARAM_PARAM_SEP);
    }

    /**
     * cookie 默认的kv分隔符
     */
    public static String COOKIE_KV_SEP = "=";
    /**
     * cookie 的多个kv对之间的分隔符
     */
    public static String COOKIE_COOKIE_SEP = ";";

    /**
     * 通过cookies获取cookie的字符串表示
     *
     * @param cookies 给定的cookie列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:08 PM
     * @since 1.0
     */
    public static String getCookieStr(Map<String, String> cookies) {
        return encapQueryString0(cookies, COOKIE_KV_SEP, COOKIE_COOKIE_SEP);
    }

    /**
     * 通过cookie格式的字符串 获取各个cookie [这里 直接使用split, 避免出现错误]
     *
     * @param cookiesStr 给定的cookie字符串
     * @return java.util.Map<java.lang.String,java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 11:09 PM
     * @since 1.0
     */
    public static Map<String, String> getCookiesByCookieStr(String cookiesStr) {
        assert0(cookiesStr != null, "'cookieStr' can't be null !");

        String[] cookies = cookiesStr.split(COOKIE_COOKIE_SEP);
        Map<String, String> res = new HashMap<>(cookies.length);
        for (int i = 0; i < cookies.length; i++) {
            String[] kvPair = cookies[i].split(COOKIE_KV_SEP);
            assert0(kvPair.length > 1, "error cookieString : '" + cookiesStr + "', around : '" + cookies[i] + "'");
            res.put(kvPair[0], kvPair[1]);
        }
        return res;
    }

    /**
     * @param e
     * @return
     * @Description: 获取给定的异常的错误信息
     * @Create at 2016-12-30 23:03:08 by '970655147'
     */
    public static String errorMsg(Exception e) {
        assert0(e != null, "'e' can't be null ");
        return e.getClass().getName() + " -> " + e.getMessage();
    }


    // add at 2016.06.18
    // 驼峰 -> 下划线表示
    private static Character underLine = '_';

    public static String camel2UnderLine(String name) {
        assert0(name != null, "'name' can't be null ");

        StringBuilder sb = new StringBuilder(name.length() + 10);
        boolean isLastCharUpper = Character.isUpperCase(name.charAt(0));
//		boolean isLastCharUpper = false;

        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (Character.isUpperCase(ch)) {
                if (!isLastCharUpper) {
                    sb.append(underLine);
                }
                sb.append(Character.toLowerCase(ch));
                isLastCharUpper = true;
            } else {
                sb.append(ch);
                isLastCharUpper = false;
            }
        }

        return sb.toString();
    }

    /**
     * 将给定的下划线命名转换为驼峰命名
     *
     * @param name 给定的下划线命名
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:12 PM
     * @since 1.0
     */
    public static String underLine2Camel(String name) {
        assert0(name != null, "'name' can't be null ");

        StringBuilder sb = new StringBuilder(name.length() + 10);
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (! underLine.equals(ch)) {
                sb.append(ch);
                continue ;
            }

            char nextCh = name.charAt(i + 1);
            if (Character.isLowerCase(nextCh) || Character.isUpperCase(nextCh)) {
                sb.append(Character.toUpperCase(name.charAt(i + 1)));

                // skip '_', the end of loop skip 'nextCh'
                i++;
            } else {
                sb.append(ch);
            }
        }

        return sb.toString();
    }

    // add at 2016.08.25

    /**
     * 根据给定的class的权限定名, 获取该class的简单名称
     *
     * @param fullName 给定的class的权限定名
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:11 PM
     * @since 1.0
     */
    public static String getClazzNameByFullName(String fullName) {
        String name = fullName;
        int lastIdxOfDot = fullName.lastIndexOf(".");
        if (lastIdxOfDot >= 0) {
            name = fullName.substring(lastIdxOfDot + 1);
        }

        return name;
    }

    // ----------------- 辅助方法 -----------------------

    /**
     * 根据给定的参数列表 和两种分隔符, 封装参数列表
     *
     * @param params    参数列表
     * @param KVSep     kv之间的分隔符
     * @param paramsSep kv对之间得分隔符
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:10 PM
     * @since 1.0
     */
    private static String encapQueryString0(Map<String, String> params, String KVSep, String paramsSep) {
        assert0(params != null, "'params' can't be null ");
        assert0(KVSep != null, "'KVSep' can't be null ");
        assert0(paramsSep != null, "'paramsSep' can't be null ");

        StringBuilder sb = new StringBuilder();
        for (Entry<String, String> entry : params.entrySet()) {
            sb.append(entry.getKey());
            sb.append(KVSep);
            sb.append(entry.getValue());
            sb.append(paramsSep);
        }
        Tools.removeLastSep(sb, paramsSep);

        return sb.toString();
    }

}
