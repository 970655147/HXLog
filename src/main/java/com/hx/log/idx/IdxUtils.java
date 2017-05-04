/**
 * file name : IdxUtils.java
 * created at : 23:06:17 2016-12-30
 * created by 970655147
 */

package com.hx.log.idx;

import com.hx.log.collection.MapUtils;
import com.hx.log.util.Tools;

import java.util.Collection;

import static com.hx.log.util.Tools.assert0;

/**
 * 索引相关的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:53 PM
 */
public final class IdxUtils {

    // disable constructor
    private IdxUtils() {
        assert0("can't instantiate !");
    }

    // ----------------- 获取索引相关方法 -----------------------

    // add at 2016.05.07

    /**
     * 根据给定的目标索引, 计算有效的索引[默认取MapUtils.GET_INFO_FROM_JSON_DEFAULT_IDX]
     *
     * @param idx   给定的索引
     * @param idxes 给定的目标索引列表
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 11:52 PM
     * @since 1.0
     */
    public static int getIdx(int idx, String[] idxes) {
        return getIdx(idx, idxes.length);
    }

    public static int getIdx(int idx, Collection<String> idxes) {
        return getIdx(idx, idxes.size());
    }

    public static int getIdx(int idx, int maxSize) {
        return getIdx(idx, maxSize, MapUtils.GET_INFO_FROM_JSON_DEFAULT_IDX);
    }

    public static int getIdx(int idx, String[] idxes, int defaultIdx) {
        return getIdx(idx, idxes.length, defaultIdx);
    }

    public static int getIdx(int idx, Collection<String> idxes, int defaultIdx) {
        return getIdx(idx, idxes.size(), defaultIdx);
    }

    public static int getIdx(int idx, int maxSize, int defaultIdx) {
        return (idx >= maxSize) ? defaultIdx : idx;
    }

    // ----------------- 待续 -----------------------

}
