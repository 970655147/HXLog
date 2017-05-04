/**
 * file name : DateUtils.java
 * created at : 23:08:54 2016-12-30
 * created by 970655147
 */

package com.hx.log.date;

import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

/**
 * 日期相关的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:37 PM
 */
public final class DateUtils {

    // disable constructor
    private DateUtils() {
        Tools.assert0("can't instantiate !");
    }


    /**
     * 获取UNIX时间戳
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static long now() {
        return System.currentTimeMillis();
    }

    /**
     * 获取UNIX时间戳对应的字符串
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String nowStr() {
        return String.valueOf(now());
    }

    /**
     * 格式化现在的时间
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String formatedNowStr() {
        return Constants.DATE_FORMAT.format(now());
    }

    /**
     * 计算现在 距离start的时候 开销的时间[ms]
     *
     * @param start 开始的时间戳
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static long spent(long start) {
        return now() - start;
    }

    /**
     * 计算现在 距离start的时候 开销的时间的字符串表示[ms]
     *
     * @param start 开始的时间戳
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String spentStr(long start) {
        return String.valueOf(spent(start));
    }

}
